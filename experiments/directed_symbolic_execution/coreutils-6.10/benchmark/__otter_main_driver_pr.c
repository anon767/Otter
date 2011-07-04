// --line-targets=$trunk/experiments/directed_symbolic_execution/coreutils-6.10/benchmark/../src/pr.c:2672
#line 2 "__otter_main_driver.c"

#include "otter/otter_builtins.h"
#include "otter/otter_fs.h"
#include "otter/otter_user.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#ifndef MAX_ARGC
#warning "MAX_ARGC not defined; using default 4"
#define MAX_ARGC 4
#endif

#ifndef MAX_ARG_LENGTHS
#warning "MAX_ARG_LENGTHS not defined; using defaults {1,10,2,2}"
#define MAX_ARG_LENGTHS 1,10,2,2
#endif

#define MAX_FILE         2
#define MAX_FILE_SIZE    8             // TODO: this should also control the size of stdin
#define MAX_FILENAME_LENGTH   5

#define MAX_ENVIRON         1
#define MAX_ENVVAR_LENGTH   20
#define MAX_ENVVAL_LENGTH   5

#define __EVAL_ERRNO    { __EVAL(errno); errno = 0; }

extern char **environ;
extern int main(int argc, char **argv);

char* __otter_environ[MAX_ENVIRON+1];

/* Allocate a char array of length (len+1), 
 * with all characters symbolic except the last one which is \0. */
char* symbolic_string(int len) {
    int i;
    char *s = malloc(len+1);

    for (i=0;i<len;i++) {
        char c; __SYMBOLIC(&c);
        s[i] = c;
    }
    s[len] = 0;
    return s;
}

#pragma cilnoremove("__otter_main_driver")
int __otter_main_driver() {
    int i;
    int argc;
    char* argv[MAX_ARGC+1];  // One for null-termination
    
    // Set up argc
    argc = 3;
    //__SYMBOLIC(&argc);
    //__ASSUME(1<=argc);
    //__ASSUME(argc<=MAX_ARGC);

    // Set up argv
    int arg_lengths[] = {MAX_ARG_LENGTHS};
    argv[0] = strdup("p");  // name of the program
    argv[1] = strdup("/e/t.t");
    argv[2] = strdup("-e");
    argv[3] = 0;

    // for (i=2;i<MAX_ARGC;i++) {
    //     argv[i] = symbolic_string(arg_lengths[i]);
    // }
    // // Use i instead of argc since i is concrete
    // // (I'm not sure if the null-termination is necessary. Seems not.)
    // argv[i] = 0;

    // Set up stdin, stdout and stderr
	__otter_fs_mount();
    setuid(__otter_UID_ROOT);
	stdin  = fopen("/dev/tty", "r"); // assert: fopen returns 0  
	stdout = fopen("/dev/tty", "w"); // assert: fopen returns 1   
	stderr = fopen("/dev/tty", "w"); // assert: fopen returns 2 

    struct __otter_fs_dnode* dnode = __otter_fs_mkdir("e", __otter_fs_root);
    // touch takes no slashes
    char* s = strdup("\b\b\b\b\b\b\b\t");
    __otter_fs_touch_with_data("t.t", dnode, s, 8);

    return main(argc, argv);
}