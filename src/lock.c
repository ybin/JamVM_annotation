/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2011, 2013
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

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/time.h>
#include <limits.h>

#include "jam.h"
#include "thread.h"
#include "hash.h"
#include "alloc.h"
#include "lock.h"
#include "symbol.h"
#include "excep.h"
#include "class.h"
#include "classlib.h"

/* Trace lock operations and inflation/deflation */
#ifdef TRACELOCK
#define TRACE(fmt, ...) jam_printf(fmt, ## __VA_ARGS__)
#else
#define TRACE(fmt, ...)
#endif

#define UN_USED -1

#define HASHTABSZE 1<<5
#define PREPARE(obj) allocMonitor(obj)
#define HASH(obj) (getObjectHashcode(obj) >> LOG_OBJECT_GRAIN)
#define COMPARE(obj, mon, hash1, hash2) hash1 == hash2 && mon->obj == obj
#define FOUND(ptr1, ptr2)                                    \
({                                                           \
     LOCKWORD_COMPARE_AND_SWAP(&ptr2->entering, UN_USED, 0); \
     ptr2;                                                   \
})

/* lockword format in "thin" mode
  63
  31                                         0
  +---------------------------------+-------+-+
  |              thread ID          | count |0|
  +---------------------------------+-------+-+
                                             ^ shape bit

  lockword format in "fat" mode
  63
  31                                         0
  +-----------------------------------------+-+
  |                 Monitor*                |1|
  +-----------------------------------------+-+
                                             ^ shape bit

  shape: 表示lock的mode是thin还是fat。
*/

#define SHAPE_BIT   0x1
#define COUNT_SIZE  8
#define COUNT_SHIFT 1
#define COUNT_MASK  (((1<<COUNT_SIZE)-1)<<COUNT_SHIFT)

#define TID_SHIFT   (COUNT_SIZE+COUNT_SHIFT)
#define TID_SIZE    (32-TID_SHIFT)
#define TID_MASK    (((1<<TID_SIZE)-1)<<TID_SHIFT)

// 清理monitor，如果它是它不再被使用，那就把它放到空闲队里中。
#define SCAVENGE(ptr)                                           \
({                                                              \
    Monitor *mon = (Monitor *)ptr;                              \
    char res = LOCKWORD_READ(&mon->entering) == UN_USED;        \
    if(res) {                                                   \
        TRACE("Scavenging monitor %p (obj %p)", mon, mon->obj); \
        mon->next = mon_free_list;                              \
        mon_free_list = mon;                                    \
    }                                                           \
    res;                                                        \
})

static Monitor *mon_free_list = NULL;
static HashTable mon_cache;

void monitorInit(Monitor *mon) {
    memset(mon, 0, sizeof(Monitor));
    pthread_mutex_init(&mon->lock, NULL);
}

// thread加入到mon的等待队列中(加到队尾，即队头的prev位置，因为是双向链表)
// mon->wait_set是一个Thread的等待队列，这个队列是一个双向列表(doubly list)
void waitSetAppend(Monitor *mon, Thread *thread) {
    if(mon->wait_set == NULL)
        mon->wait_set = thread->wait_prev = thread;

    thread->wait_next = mon->wait_set;
    thread->wait_prev = mon->wait_set->wait_prev;
    thread->wait_prev->wait_next = mon->wait_set->wait_prev = thread;

    thread->wait_id = mon->wait_count++;
}

// 把thread从mon的等待队列中取出(unlink)
void waitSetUnlinkThread(Monitor *mon, Thread *thread) {
    if(mon->wait_set == thread)
        if((mon->wait_set = mon->wait_set->wait_next) == thread)
            mon->wait_set = NULL;

    thread->wait_prev->wait_next = thread->wait_next;
    thread->wait_next->wait_prev = thread->wait_prev;
    thread->wait_prev = thread->wait_next = NULL;
}

// 取出mon的等待队列中的第一个node，然后signal it。
Thread *waitSetSignalNext(Monitor *mon) {
    Thread *thread = mon->wait_set;

    if(thread != NULL) {
        waitSetUnlinkThread(mon, thread);
        pthread_cond_signal(&thread->wait_cv);

        thread->notify_id = mon->wait_count;
    }

    return thread;
}

// lock the monitor
void monitorLock(Monitor *mon, Thread *self) {
    if(mon->owner == self)
        mon->count++;
    else {
		// pthread_mutex_trylock跟pthread_mutex_lock的不同之处在于:
		// 当lock获取失败时，pthread_mutex_trylock返回non-zero值，
		// 而pthread_mutex_lock导致线程blocked直到lock可用。
        if(pthread_mutex_trylock(&mon->lock)) {
			// 获取lock失败，本线程已经做好被挂起(blocked)的觉悟了！
            disableSuspend(self);

			// 遗言：本线程是被mon这个lock block的，而且已经被block了blocked_count次了。
            self->blocked_mon = mon;
            self->blocked_count++;
            classlibSetThreadState(self, BLOCKED);
			// 再见了，朋友们，我要全力一拼了，即使是飞蛾扑火也要勇敢前行。。。
            pthread_mutex_lock(&mon->lock);
			
			// ... ... 此处可能被block很久 ... ...
			
            classlibSetThreadState(self, RUNNING);
			// 我胡汉三又回来啦！
            self->blocked_mon = NULL;

            enableSuspend(self);
        }
		// mon是属于我的。。。
        mon->owner = self;
    }
}

// 能获得mon的芳心最好，否则。。。 天涯何处无芳草啊，爷我另找歪脖子树去，走起。。。
int monitorTryLock(Monitor *mon, Thread *self) {
    if(mon->owner == self)
        mon->count++;
    else {
        if(pthread_mutex_trylock(&mon->lock))
            return FALSE;
        mon->owner = self;
    }

    return TRUE;
}

// 多次获取lock，只有最后一次释放时才会真的unlock。一次次，我伤了她的心。。。
void monitorUnlock(Monitor *mon, Thread *self) {
    if(mon->owner == self) {
        if(mon->count == 0) {
            mon->owner = NULL;
            pthread_mutex_unlock(&mon->lock);
        } else
            mon->count--;
    }
}

int monitorWait(Monitor *mon, Thread *self, long long ms, int ns,
                int is_wait, int interruptible) {
    char interrupted = interruptible && self->interrupted;

    /* Check we own the monitor */
    if(mon->owner != self)
        return FALSE;

    if(!interrupted) {
        char timed = (ms != 0) || (ns != 0);
        char timeout = FALSE;
        struct timespec ts;
        int old_count;
        int state;

        disableSuspend(self);

        /* Unlock the monitor.  As it could be recursively
           locked remember the recursion count */
        old_count = mon->count;
        mon->owner = NULL;
        mon->count = 0;

        /* Counter used in thin-lock deflation */
        mon->in_wait++;

        self->wait_mon = mon;

        state = timed ? (is_wait ? OBJECT_TIMED_WAIT : SLEEPING)
                      : (is_wait ? OBJECT_WAIT : BLOCKED);

        classlibSetThreadState(self, state);

        if(state == BLOCKED) {
            self->blocked_mon = mon;
            self->blocked_count++;
        } else
            self->waited_count++;

        self->interrupting = FALSE;

        /* Add the thread onto the end of the wait set */
        waitSetAppend(mon, self);

        if(timed)
            getTimeoutRelative(&ts, ms, ns);

        while(self->wait_next != NULL && !self->interrupting && !timeout)
            if(timed) {
                timeout = pthread_cond_timedwait(&self->wait_cv,
                              &mon->lock, &ts) == ETIMEDOUT;

                /* On Linux/i386 systems using LinuxThreads,
                   pthread_cond_timedwait is implemented using
                   sigjmp/longjmp.  This resets the fpu control
                   word back to 64-bit precision.  The macro is
                   empty for sane platforms. */ 

                FPU_HACK;
            } else
                pthread_cond_wait(&self->wait_cv, &mon->lock);

        /* If we've been interrupted or timed-out, we will not have been
           removed from the wait set.  If we have, we must have been
           notified afterwards.  In this case, the notify has been lost,
           and we must signal another thread */

        if(self->interrupting || timeout) {
            /* An interrupt after a timeout remains pending */
            interrupted = interruptible && !timeout;

            if(self->wait_next != NULL)
                waitSetUnlinkThread(mon, self);
            else {
                /* Notify lost.  Signal another thread only if it
                   was on the wait set at the time of the notify */
                if(mon->wait_set != NULL &&
                                mon->wait_set->wait_id < self->notify_id) {
                    Thread *thread = waitSetSignalNext(mon);
                    thread->notify_id = self->notify_id;
                }
            }
        }

        classlibSetThreadState(self, RUNNING);
        self->wait_mon = NULL;

        if(state == BLOCKED)
            self->blocked_mon = NULL;

        /* Restore the monitor owner and recursion count */
        mon->count = old_count;
        mon->owner = self;
        mon->in_wait--;

        enableSuspend(self);
    }

    if(interrupted) {
        self->interrupted = FALSE;
        signalException(java_lang_InterruptedException, NULL);
    }

    return TRUE;
}

// 把mon的等待队列中的第一个thread唤醒，它是等待时间最长的。
int monitorNotify(Monitor *mon, Thread *self) {
    if(mon->owner != self)
        return FALSE;

    /* Signal the first thread in the wait set.  This
       is the thread which has been waiting the longest */
    waitSetSignalNext(mon);

    return TRUE;
}

// 把mon的等待队列中所有thread都唤醒，你们自己竞争去吧! 扔个肥皂，赶紧溜。。。
int monitorNotifyAll(Monitor *mon, Thread *self) {
    if(mon->owner != self)
        return FALSE;

    /* Signal all threads in the wait set */
    while(waitSetSignalNext(mon) != NULL);

    return TRUE;
}

// 创建monitor，如果空闲列表中有闲置的就用闲置的。
Monitor *allocMonitor(Object *obj) {
    Monitor *mon;

    if(mon_free_list != NULL) {
        mon = mon_free_list;
        mon_free_list = mon->next;      
    } else {
        mon = sysMalloc(sizeof(Monitor));
        monitorInit(mon);
    }
	// monitor是为object分配的哦。
    mon->obj = obj;
    /* No need to wrap in LOCKWORD_WRITE as no thread should
     * be modifying it when it's on the free list */
    mon->entering = 0;
    return mon;
}

Monitor *findMonitor(Object *obj) {
    uintptr_t lockword = LOCKWORD_READ(&obj->lock);

    if(lockword & SHAPE_BIT)
        return (Monitor*) (lockword & ~SHAPE_BIT);
    else {
        Monitor *mon;
        /* Add if absent, scavenge, locked */
        findHashEntry(mon_cache, obj, mon, TRUE, TRUE, TRUE);
        return mon;
    }
}

// thin lock转换为fat lock
static void inflate(Object *obj, Monitor *mon, Thread *self) {
    TRACE("Thread %p is inflating obj %p...\n", self, obj);
    clearFlcBit(obj);
    monitorNotifyAll(mon, self); // inflate之前lock是thin的，所以这个只是清空等待队列而已，它本就应该是空的吧
    LOCKWORD_WRITE(&obj->lock, (uintptr_t) mon | SHAPE_BIT); // 转换为fat lock
}

void objectLock(Object *obj) {
    Thread *self = threadSelf();
    uintptr_t thin_locked = self->id<<TID_SHIFT;
    uintptr_t entering, lockword;
    Monitor *mon;

    TRACE("Thread %p lock on obj %p...\n", self, obj);

    if(LOCKWORD_COMPARE_AND_SWAP(&obj->lock, 0, thin_locked)) {
        /* This barrier is not needed for the thin-locking implementation;
           it's a requirement of the Java memory model. */
        JMM_LOCK_MBARRIER();
        return;
    }

    lockword = LOCKWORD_READ(&obj->lock);
    if((lockword & (TID_MASK|SHAPE_BIT)) == thin_locked) {
        int count = lockword & COUNT_MASK;

        if(count < (((1<<COUNT_SIZE)-1)<<COUNT_SHIFT))
            LOCKWORD_WRITE(&obj->lock, lockword + (1<<COUNT_SHIFT));
        else {
            mon = findMonitor(obj);
            monitorLock(mon, self);
            inflate(obj, mon, self);
            mon->count = 1<<COUNT_SIZE;
        }
        return;
    }

try_again:
    mon = findMonitor(obj);

try_again2:
    if((entering = LOCKWORD_READ(&mon->entering)) == UN_USED)
        goto try_again;

    if(!(LOCKWORD_COMPARE_AND_SWAP(&mon->entering, entering, entering+1)))
        goto try_again2;

    if(mon->obj != obj) {
        while(entering = LOCKWORD_READ(&mon->entering),
                        !(LOCKWORD_COMPARE_AND_SWAP(&mon->entering,
                                                    entering, entering-1)));
        goto try_again;
    }

    monitorLock(mon, self);

    while(entering = LOCKWORD_READ(&mon->entering),
                    !(LOCKWORD_COMPARE_AND_SWAP(&mon->entering,
                                                entering, entering-1)));

    while((LOCKWORD_READ(&obj->lock) & SHAPE_BIT) == 0) {
        setFlcBit(obj);

        if(LOCKWORD_COMPARE_AND_SWAP(&obj->lock, 0, thin_locked))
            inflate(obj, mon, self);
        else
            monitorWait(mon, self, 0, 0, FALSE, FALSE);
    }
}

void objectUnlock(Object *obj) {
    Thread *self = threadSelf();
    uintptr_t lockword = LOCKWORD_READ(&obj->lock);
    uintptr_t thin_locked = self->id<<TID_SHIFT;

    TRACE("Thread %p unlock on obj %p...\n", self, obj);

    if(lockword == thin_locked) {
        /* This barrier is not needed for the thin-locking implementation;
           it's a requirement of the Java memory model. */
        JMM_UNLOCK_MBARRIER();
        LOCKWORD_WRITE(&obj->lock, 0);

        /* Required by thin-locking mechanism. */
        MBARRIER();

retry:
        if(testFlcBit(obj)) {
            Monitor *mon = findMonitor(obj);

            if(!monitorTryLock(mon, self)) {
                threadYield(self);
                goto retry;
            }

            if(testFlcBit(obj) && (mon->obj == obj))
                monitorNotify(mon, self);

            monitorUnlock(mon, self);
        }
    } else {
        if((lockword & (TID_MASK|SHAPE_BIT)) == thin_locked)
            LOCKWORD_WRITE(&obj->lock, lockword - (1<<COUNT_SHIFT));
        else
            if((lockword & SHAPE_BIT) != 0) {
                Monitor *mon = (Monitor*) (lockword & ~SHAPE_BIT);

                if((mon->count == 0) && (LOCKWORD_READ(&mon->entering) == 0) &&
                                (mon->in_wait == 0)) {
                    TRACE("Thread %p is deflating obj %p...\n", self, obj);

                    /* This barrier is not needed for the thin-locking
                       implementation; it's a requirement of the Java
                       memory model. */
                    JMM_UNLOCK_MBARRIER();

                    LOCKWORD_WRITE(&obj->lock, 0);
                    LOCKWORD_COMPARE_AND_SWAP(&mon->entering, 0, UN_USED);
                }

                monitorUnlock(mon, self);
            }
    }
}

void objectWait(Object *obj, long long ms, int ns, int interruptible) {
    uintptr_t lockword = LOCKWORD_READ(&obj->lock);
    Thread *self = threadSelf();
    Monitor *mon;

    TRACE("Thread %p Wait on obj %p...\n", self, obj);

    if((lockword & SHAPE_BIT) == 0) {
        int tid = (lockword&TID_MASK)>>TID_SHIFT;
        if(tid == self->id) {
            mon = findMonitor(obj);
            monitorLock(mon, self);
            inflate(obj, mon, self);
            mon->count = (lockword&COUNT_MASK)>>COUNT_SHIFT;
        } else
            goto not_owner;
    } else
        mon = (Monitor*) (lockword & ~SHAPE_BIT);

    if(monitorWait(mon, self, ms, ns, TRUE, interruptible))
        return;

not_owner:
    signalException(java_lang_IllegalMonitorStateException,
                    "thread not owner");
}

void objectNotify(Object *obj) {
    uintptr_t lockword = LOCKWORD_READ(&obj->lock);
    Thread *self = threadSelf();

    TRACE("Thread %p Notify on obj %p...\n", self, obj);

    if((lockword & SHAPE_BIT) == 0) {
		// 如果是thin lock，直接退出，thin不会导致等待线程挂起(blocked)，所以无需唤醒。
        int tid = (lockword&TID_MASK)>>TID_SHIFT;
        if(tid == self->id) // 只有lock的owner才能释放它，如果别人能随便释放lock，岂不乱套了啊！
            return;
    } else {
    	// 如果是fat lock，就需要notify等待队列中的某个thread
        Monitor *mon = (Monitor*) (lockword & ~SHAPE_BIT);
        if(monitorNotify(mon, self))
            return;
    }

    signalException(java_lang_IllegalMonitorStateException,
                    "thread not owner");
}

void objectNotifyAll(Object *obj) {
    uintptr_t lockword = LOCKWORD_READ(&obj->lock);
    Thread *self = threadSelf();

    TRACE("Thread %p NotifyAll on obj %p...\n", self, obj);

    if((lockword & SHAPE_BIT) == 0) {
		// thin lock，直接退出
        int tid = (lockword&TID_MASK)>>TID_SHIFT;
        if(tid == self->id)
            return;
    } else {
    	// fat lock，唤醒等待队列中的所有线程
        Monitor *mon = (Monitor*) (lockword & ~SHAPE_BIT);
        if(monitorNotifyAll(mon, self))
            return;
    }

    signalException(java_lang_IllegalMonitorStateException,
                    "thread not owner");
}

// 判断object是否被当前线程锁定了
int objectLockedByCurrent(Object *obj) {
    uintptr_t lockword = LOCKWORD_READ(&obj->lock);
    Thread *self = threadSelf();

    if((lockword & SHAPE_BIT) == 0) {
        int tid = (lockword&TID_MASK)>>TID_SHIFT;
        if(tid == self->id)
            return TRUE;
    } else {
        Monitor *mon = (Monitor*) (lockword & ~SHAPE_BIT);
        if(mon->owner == self)
            return TRUE;
    }
    return FALSE;
}

// 获取锁定obj的线程对象
Thread *objectLockedBy(Object *obj) {
    uintptr_t lockword = LOCKWORD_READ(&obj->lock);
    Thread *owner;

    if((lockword & SHAPE_BIT) == 0) {
        int tid = (lockword&TID_MASK)>>TID_SHIFT;
        owner = findRunningThreadByTid(tid);
    } else {
        Monitor *mon = (Monitor*) (lockword & ~SHAPE_BIT);
        owner = mon->owner;
    }
    return owner;
}

int initialiseMonitor() {
    /* Init hash table, create lock */
    initHashTable(mon_cache, HASHTABSZE, TRUE);

    return TRUE;
}

/* Heap compaction support */

#define ITERATE(ptr) {               \
    Monitor *mon = (Monitor*)ptr;    \
    if(isMarked(mon->obj))           \
         threadReference(&mon->obj); \
}

void threadMonitorCache() {
   hashIterate(mon_cache);
}
