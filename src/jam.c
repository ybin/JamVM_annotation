/*
 * Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
 * 2013, 2014 Robert Lougher <rob@jamvm.org.uk>.
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
#include <stdlib.h>
#include <stdarg.h>

#include "jam.h"
#include "hash.h"
#include "class.h"
#include "symbol.h"
#include "excep.h"
#include "thread.h"
#include "classlib.h"

#ifdef USE_ZIP
#define BCP_MESSAGE "<jar/zip files and directories separated by :>"
#else
#define BCP_MESSAGE "<directories separated by :>"
#endif

void showNonStandardOptions() {
    printf("  -Xbootclasspath:%s\n", BCP_MESSAGE);
    printf("\t\t   locations where to find the system classes\n");
    printf("  -Xbootclasspath/a:%s\n", BCP_MESSAGE);
    printf("\t\t   locations are appended to the bootstrap class path\n");
    printf("  -Xbootclasspath/p:%s\n", BCP_MESSAGE);
    printf("\t\t   locations are prepended to the bootstrap class path\n");
    printf("  -Xbootclasspath/c:%s\n", BCP_MESSAGE);
    printf("\t\t   locations where to find Classpath's classes\n");
    printf("  -Xbootclasspath/v:%s\n", BCP_MESSAGE);
    printf("\t\t   locations where to find JamVM's classes\n");
    printf("  -Xasyncgc\t   turn on asynchronous garbage collection\n");
    printf("  -Xcompactalways  always compact the heap when garbage-collecting\n");
    printf("  -Xnocompact\t   turn off heap-compaction\n");
#ifdef INLINING
    printf("  -Xnoinlining\t   turn off interpreter inlining\n");
    printf("  -Xshowreloc\t   show opcode relocatability\n");
    printf("  -Xreplication:[none|always|<value>]\n");
    printf("\t\t   none : always re-use super-instructions\n");
    printf("\t\t   always : never re-use super-instructions\n");
    printf("\t\t   <value> copy when usage reaches threshold value\n");
    printf("  -Xcodemem:[unlimited|<size>] (default maximum heapsize/4)\n");
#endif
    printf("  -Xms<size>\t   set the initial size of the heap\n");
    printf("\t\t   (default = MAX(physical memory/64, %dM))\n",
           DEFAULT_MIN_HEAP/MB);
    printf("  -Xmx<size>\t   set the maximum size of the heap\n");
    printf("\t\t   (default = MIN(physical memory/4, %dM))\n",
           DEFAULT_MAX_HEAP/MB);
    printf("  -Xss<size>\t   set the Java stack size for each thread "
           "(default = %dK)\n", DEFAULT_STACK/KB);
    printf("\t\t   size may be followed by K,k or M,m (e.g. 2M)\n");
}

void showUsage(char *name) {
    printf("Usage: %s [-options] class [arg1 arg2 ...]\n", name);
    printf("                 (to run a class file)\n");
    printf("   or  %s [-options] -jar jarfile [arg1 arg2 ...]\n", name);
    printf("                 (to run a standalone jar file)\n");
    printf("\nwhere options include:\n");
    printf("  -client\t   compatibility (ignored)\n");
    printf("  -server\t   compatibility (ignored)\n\n");
    printf("  -cp\t\t   <jar/zip files and directories separated by :>\n");
    printf("  -classpath\t   <jar/zip files and directories separated by :>\n");
    printf("\t\t   locations where to find application classes\n");
    printf("  -D<name>=<value> set a system property\n");
    printf("  -verbose[:class|gc|jni]\n");
    printf("\t\t   :class print out information about class loading, etc.\n");
    printf("\t\t   :gc print out results of garbage collection\n");
    printf("\t\t   :jni print out native method dynamic resolution\n");
    printf("  -version\t   print out version number and copyright information\n");
    printf("  -showversion     show version number and copyright and continue\n");
    printf("  -fullversion     show jpackage-compatible version number and exit\n");
    printf("  -? -help\t   print out this message\n");
    printf("  -X\t\t   show help on non-standard options\n");
}

void showVersionAndCopyright() {
    printf("java version \"%s\"\n", JAVA_COMPAT_VERSION);
    printf("JamVM version %s\n", VERSION);
    printf("Copyright (C) 2003-2014 Robert Lougher <rob@jamvm.org.uk>\n\n");
    printf("This program is free software; you can redistribute it and/or\n");
    printf("modify it under the terms of the GNU General Public License\n");
    printf("as published by the Free Software Foundation; either version 2,\n");
    printf("or (at your option) any later version.\n\n");
    printf("This program is distributed in the hope that it will be useful,\n");
    printf("but WITHOUT ANY WARRANTY; without even the implied warranty of\n");
    printf("MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n");
    printf("GNU General Public License for more details.\n");
    printf("\nBuild information:\n\nExecution Engine: %s\n",
           getExecutionEngineName());

#if defined(__GNUC__) && defined(__VERSION__)
    printf("Compiled with: gcc %s\n", __VERSION__);
#endif

    printf("\nBoot Library Path: %s\n", classlibDefaultBootDllPath());
    printf("Boot Class Path: %s\n", classlibDefaultBootClassPath());
}

void showFullVersion() {
    printf("java full version \"jamvm-%s\"\n", JAVA_COMPAT_VERSION);
}

// 解析argv[]并设置相关数据到args对象中，这部分可以参考(Oracle) java.exe的用法，他们是兼容的。
int parseCommandLine(int argc, char *argv[], InitArgs *args) {
	// 键值对
    Property props[argc-1];
    int is_jar = FALSE;
    int status = 0;
    int i;

    args->commandline_props = &props[0];

    for(i = 1; i < argc; i++) {
		// 对于具体的选项，都是以'-'开头，所以不是以'-'开头的参数就是VM要执行的class name。
		// jamvm -option value -option value ... <qualified class name>
		// e.g.
		// jamvm -d32 -client com.example.MainClass
        if(*argv[i] != '-') {
            if(args->min_heap > args->max_heap) {
                printf("Minimum heap size greater than max!\n");
                status = 1;
                goto exit;
            }

			// 保存property值，注意: class name一定要放到最后，否则后面的参数就无法解析了。
            if(args->props_count) {
                args->commandline_props = sysMalloc(args->props_count *
                                                    sizeof(Property));
                memcpy(args->commandline_props, &props[0], args->props_count *
                                                           sizeof(Property));
            }

			// class name不一定是class，也可以是可执行的jar文件名
            if(is_jar) {
                args->classpath = argv[i];
                argv[i] = "jamvm/java/lang/JarLauncher";
            }

			// 最后返回class name在argv[]中对应的索引
            return i;
        }

		/* 参数有几种不同的格式:
		   1. 开关类型的。如-client, -d64, -help, -version等
		   2. 参数名紧跟值类型的。如-verbose:[class|gc|jni], -splash:<imagepath>等
		   3. 参数名+空格+值类型的。如-cp "xxx"，这种类型解析时i会自动增加

		   所以，结论就是每轮for循环，如果参数不是以'-'开头，那么该参数一定是class name。
		*/
        switch(parseCommonOpts(argv[i], args, FALSE)) {
            case OPT_OK:
                break;
        	
            case OPT_ERROR:
        	status = 1;
        	goto exit;

			// parseCommonOpts()只解析部分参数，如-client, -d32等，剩下的在下面解析
            case OPT_UNREC:
            default:
                if(strcmp(argv[i], "-?") == 0 ||
                   strcmp(argv[i], "-help") == 0) {
                   // 显示帮助信息
                    goto usage;

                } else if(strcmp(argv[i], "-X") == 0) {
                	// 显示非标准参数信息，如-Xss(stack size)
                    showNonStandardOptions();
                    goto exit;

                } else if(strcmp(argv[i], "-version") == 0) {
                	// 打印版本信息并退出
                    showVersionAndCopyright();
                    goto exit;

                } else if(strcmp(argv[i], "-showversion") == 0) {
                	// 打印版本信息并继续
                    showVersionAndCopyright();
        
                } else if(strcmp(argv[i], "-fullversion") == 0) {
                	// 显示完整信息并退出
                    showFullVersion();
                    goto exit;

                } else if(strncmp(argv[i], "-verbose", 8) == 0) {
                	// 启用详细输出，-verbose:[class|gc|jni]，冒号后面表示打印级别
                    char *type = &argv[i][8];

                    if(*type == '\0' || strcmp(type, ":class") == 0)
                        args->verboseclass = TRUE;

                    else if(strcmp(type, ":gc") == 0 || strcmp(type, "gc") == 0)
                        args->verbosegc = TRUE;

                    else if(strcmp(type, ":jni") == 0)
                        args->verbosedll = TRUE;

                } else if(strcmp(argv[i], "-jar") == 0) {
                    is_jar = TRUE;

                } else if(strcmp(argv[i], "-classpath") == 0 ||
                          strcmp(argv[i], "-cp") == 0) {
					// 设置class path
                    if(i == argc - 1) {
                        printf("%s : missing path list\n", argv[i]);
                        goto exit;
                    }
					// 注意这里的 i 自增了
                    args->classpath = argv[++i];

                } else if(strncmp(argv[i], "-Xbootclasspath/c:", 18) == 0) {
                	// 冒号类型的参数，直接取冒号后面的字符串
                    args->bootpath_c = argv[i] + 18;

                } else if(strncmp(argv[i], "-Xbootclasspath/v:", 18) == 0) {
                    args->bootpath_v = argv[i] + 18;

                /* Compatibility options */
                } else if(strcmp(argv[i], "-client") == 0 ||
                          strcmp(argv[i], "-server") == 0) {
                    /* Ignore */
                } else {
                    printf("Unrecognised command line option: %s\n", argv[i]);
                    status = 1;
                    goto usage;
                }
        }
    }

usage:
    showUsage(argv[0]);

exit:
    exit(status);
}

int main(int argc, char *argv[]) {
    Class *array_class, *main_class;
    Object *system_loader, *array;
    MethodBlock *mb;
    InitArgs args;
    int class_arg;
    char *cpntr;
    int status;
    int i;

	// 设置参数默认值
    setDefaultInitArgs(&args);
	// 解析命令行参数，并覆盖参数默认值
    class_arg = parseCommandLine(argc, argv, &args);

	// 设置运行时栈，stack
    args.main_stack_base = &array_class;

	// 根据上述参数，初始化虚拟机
    if(!initVM(&args)) {
        printf("Could not initialise VM.  Aborting.\n");
        exit(1);
    }

	// 设置类加载器，它会调用API lib中的java.lang.ClassLoader.getSystemClassLoader()
	// 来获取class loader对象
   if((system_loader = getSystemClassLoader()) == NULL)
        goto error;

    mainThreadSetContextClassLoader(system_loader);

	// 把qualified class name里的"."替换为"/"
    for(cpntr = argv[class_arg]; *cpntr; cpntr++)
        if(*cpntr == '.')
            *cpntr = '/';

	// 根据class name，找到main class对象
    main_class = findClassFromClassLoader(argv[class_arg], system_loader);
	// 初始化main class
    if(main_class != NULL)
        initClass(main_class);

    if(exceptionOccurred())
        goto error;

	// 找到main函数
    mb = lookupMethod(main_class, SYMBOL(main),
                                  SYMBOL(_array_java_lang_String__V));

    if(mb == NULL || !(mb->access_flags & ACC_STATIC)) {
        signalException(java_lang_NoSuchMethodError, "main");
        goto error;
    }

	// 整理字符串数组，为main函数的参数作准备
    /* Create the String array holding the command line args */
    i = class_arg + 1;
    if((array_class = findArrayClass(SYMBOL(array_java_lang_String))) &&
           (array = allocArray(array_class, argc - i, sizeof(Object*))))  {
        Object **args = ARRAY_DATA(array, Object*) - i;

        for(; i < argc; i++)
            if(!(args[i] = Cstr2String(argv[i])))
                break;

		// 调用main函数
        /* Call the main method */
        if(i == argc)
            executeStaticMethod(main_class, mb, array);
    }

error:
    /* ExceptionOccurred returns the exception or NULL, which is OK
       for normal conditionals, but not here... */
    if((status = exceptionOccurred() ? 1 : 0))
        uncaughtException();

	// 结束deamon线程，并退出虚拟机
    /* Wait for all but daemon threads to die */
    mainThreadWaitToExitVM();
    exitVM(status);

   /* Keep the compiler happy */
    return 0;
}
