#include "otter/otter_fs.h"
#include "otter/otter_user.h"
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

// The current setting is the same as KLEE
#define PROGRAM_NAME     "program"
#define MAX_ARGC         4
#define MAX_ARG_LENGTHS  {8, 2, 2, 10} 
#define MAX_FILE         2
#define MAX_FILE_SIZE    8             // TODO: this should also control the size of stdin
#define MAX_FILENAME_LENGTH   5

#define MAX_ENVIRON         1
#define MAX_ENVVAR_LENGTH   20
#define MAX_ENVVAL_LENGTH   5

extern char **environ;
extern int main(int argc, char **argv);

char* __otter_environ[MAX_ENVIRON+1];

#pragma cilnoremove("__FAILURE")
void __FAILURE() {}

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
    // TODO: symbolic argc
    // __SYMBOLIC(&argc);
    // __ASSUME(1<=argc);
    // __ASSUME(argc<=MAX_ARGC);
    argc = MAX_ARGC;

    // Set up argv
    int arg_lengths[] = MAX_ARG_LENGTHS;
    for (i=0;i<MAX_ARGC;i++) {
        argv[i] = symbolic_string(arg_lengths[i]);
    }
    argv[MAX_ARGC] = 0;

    // Set up stdin, stdout and stderr
	__otter_fs_mount();
    setuid(__otter_UID_ROOT);
	stdin  = fopen("/dev/tty", "r"); // assert: fopen returns 0  
	stdout = fopen("/dev/tty", "w"); // assert: fopen returns 1   
	stderr = fopen("/dev/tty", "w"); // assert: fopen returns 2 

    // Set up the file system
    struct __otter_fs_dnode* dnode = __otter_fs_mkdir("etc", __otter_fs_root);
    __otter_fs_touch("/etc/file", dnode);
    FILE* f = fopen("/etc/file", "w");
    fprintf(f,"Hello world");
    fclose(f);
    
    // Set up env
    for (i=0;i<MAX_ENVIRON;i++)
        __otter_environ[i] = symbolic_string(MAX_ENVVAR_LENGTH + 1 + MAX_ENVVAL_LENGTH);  // "name=value"
    __otter_environ[MAX_ENVIRON] = 0;
    environ = &__otter_environ[0];

    return main(argc, argv);
}
