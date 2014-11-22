/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2011
 * Robert Lougher <rob@jamvm.org.uk>.
 *
 * This file is part of JamVM.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2,
 * or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "thread.h"

// 一个hash entry就是"数据指针"+hash值
typedef struct hash_entry {
    void *data;
    int hash;
} HashEntry;

/*
      hash table
	+------------+	   hash table data
	| hash_table | ---> +----------+ \
	+------------+		| data ptr | | hash entry
	| hash_size  |		| hash val | |
	+------------+		+----------+ /	      hashed object
	| hash_count |		| data ptr | ---> +------------------+
	+------------+		| hash val |	  | 实际被hash的对象 |
	|    lock    |		+----------+	  +------------------+
	+------------+		|other ents|
						+----------+
	hash_size: 哈希表的大小
	hash_count: 实际保存的哈希对象的个数
	线性探测法构建的哈希表在增加hash entry时，如果空闲slot不足会自动扩容，
	所以哈希表中保证*一定*会有空闲的slot。
 */

typedef struct hash_table {
    HashEntry *hash_table;
    int hash_size;
    int hash_count;
    VMLock lock;
} HashTable;

extern void resizeHash(HashTable *table, int new_size);
extern void lockHashTable0(HashTable *table, Thread *self);
extern void unlockHashTable0(HashTable *table, Thread *self);

// 线性探测法实现的哈希表
#define initHashTable(table, initial_size, create_lock)                \
{                                                                      \
    table.hash_table = gcMemMalloc(sizeof(HashEntry)*initial_size);    \
    memset(table.hash_table, 0, sizeof(HashEntry)*initial_size);       \
    table.hash_size = initial_size;                                    \
    table.hash_count = 0;                                              \
    if(create_lock)                                                    \
        initVMLock(table.lock);                                        \
}

#define lockHashTable(table)                                           \
    lockHashTable0(&table, threadSelf());

#define unlockHashTable(table)                                         \
    unlockHashTable0(&table, threadSelf());

#define hashTableCount(table)                                          \
    table.hash_count

// ptr相当于key，ptr2相当于value
#define findHashEntry(table, ptr, ptr2, add_if_absent, scavenge,       \
                      locked)                                          \
{                                                                      \
    int hash = HASH(ptr);  /* hash值 */                                            \
    int i;                  /* 索引 */                                           \
                                                                       \
    Thread *self;                                                      \
    if(locked) {                                                       \
        self = threadSelf();                                           \
        lockHashTable0(&table, self);                                  \
    }                                                                  \
                                                                       \
    i = hash & (table.hash_size - 1);                                  \
                                                                       \
    for(;;) {                                                          \
        ptr2 = table.hash_table[i].data;                               \
		/* 如果第i个slot未被占用，或者被占用但是恰好是我们要找到对象 */ \
		if((ptr2 == NULL) ||                                           \
                 (COMPARE(ptr, ptr2, hash, table.hash_table[i].hash))) \
            break;                                                     \
        /* 否则，寻找下一个slot，即进行线性探测 */                      \
        i = (i+1) & (table.hash_size - 1);                             \
    }                                                                  \
                                                                       \
    if(ptr2) {                                                         \
        ptr2 = FOUND(ptr, ptr2);                                       \
    } else                                                             \
        if(add_if_absent) {                                            \
			/* 如果slot是空的，那填补上一个 */							\
            table.hash_table[i].hash = hash;                           \
            ptr2 = table.hash_table[i].data = PREPARE(ptr);            \
                                                                       \
            if(ptr2) {                                                 \
                table.hash_count++;                                    \
                if((table.hash_count * 4) > (table.hash_size * 3)) {   \
					/* 如果空闲slot的数量小于等于hash_size的1/4了，就要resize哈希表 */ \
                    int new_size;                                      \
					/* 如果需要清理，那就先清理 */						\
					if(scavenge) {                                     \
                        HashEntry *entry = table.hash_table;           \
                        int cnt = table.hash_count;                    \
                        for(; cnt; entry++) {                          \
                            void *data = entry->data;                  \
                            if(data) {                                 \
                                if(SCAVENGE(data)) {                   \
                                    entry->data = NULL;                \
                                    table.hash_count--;                \
                                }                                      \
                                cnt--;                                 \
                            }                                          \
                        }                                              \
						/* 清理完之后，如果空闲slot仍然小于等于hash_size的1/3，那就resize，否则就不用 */ \
						if((table.hash_count * 3) >                    \
                                        (table.hash_size * 2))         \
                            new_size = table.hash_size*2;              \
                        else                                           \
                            new_size = table.hash_size;                \
                    } else   /* 如果无需清理，那就直接resize为原来的两倍 */ \
                        new_size = table.hash_size*2;                  \
                                                                       \
                    resizeHash(&table, new_size);                      \
                }                                                      \
            }                                                          \
        }                                                              \
                                                                       \
    if(locked)                                                         \
        unlockHashTable0(&table, self);                                \
}

#define deleteHashEntry(table, ptr, locked)                            \
{                                                                      \
    int hash = HASH(ptr);                                              \
    void *ptr2;                                                        \
    int i;                                                             \
                                                                       \
    Thread *self;                                                      \
    if(locked) {                                                       \
        self = threadSelf();                                           \
        lockHashTable0(&table, self);                                  \
    }                                                                  \
                                                                       \
    i = hash & (table.hash_size - 1);                                  \
                                                                       \
    for(;;) {                                                          \
        ptr2 = table.hash_table[i].data;                               \
        if((ptr2 == NULL) ||                                           \
                 (COMPARE(ptr, ptr2, hash, table.hash_table[i].hash))) \
            break;                                                     \
                                                                       \
        i = (i+1) & (table.hash_size - 1);                             \
    }                                                                  \
                                                                       \
    if(ptr2)                                                           \
        table.hash_table[i].data = DELETED;                            \
                                                                       \
    if(locked)                                                         \
        unlockHashTable0(&table, self);                                \
}

// (倒序)遍历哈希表
#define hashIterate(table)                                             \
{                                                                      \
    HashEntry *_entry = table.hash_table;                              \
    int _cnt = table.hash_count;                                       \
                                                                       \
    while(_cnt) {                                                      \
        void *_data = _entry++->data;                                  \
        if(_data) {                                                    \
            ITERATE(_data);                                            \
            _cnt--;                                                    \
        }                                                              \
    }                                                                  \
}

// (正序)遍历哈希表
#define hashIterateP(table)                                            \
{                                                                      \
    HashEntry *entry = table.hash_table;                               \
    int cnt = table.hash_count;                                        \
                                                                       \
    while(cnt) {                                                       \
        void **data_pntr = &entry++->data;                             \
        if(*data_pntr) {                                               \
            ITERATE(data_pntr);                                        \
            cnt--;                                                     \
        }                                                              \
    }                                                                  \
}

// 释放哈希表
#define freeHashTable(table)                                           \
    gcMemFree(table.hash_table)

#define gcFreeHashTable(table)                                         \
    freeHashTable(table)
