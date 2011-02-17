#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

// The current setting is the same as KLEE
#define PROGRAM_NAME     "program"
#define MAX_ARG          3             // Excluding argv[0]===PROGRAM_NAME
#define MAX_ARG_LENGTHS  {8, 10, 2, 2} 
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
    int argc;
    char* argv[MAX_ARG+1];
    int i;
    
    // Set up argc
    __SYMBOLIC(&argc);
    __ASSUME(1<=argc);
    __ASSUME(argc<=MAX_ARG);

    // Set up argv
    int arg_lengths[] = MAX_ARG_LENGTHS;
    for (i=0;i<=MAX_ARG;i++) {
        argv[i] = symbolic_string(arg_lengths[i]);
    }

    // Set up stdin, stdout and stderr
	__otter_fs_mount();
	int stdin  = fopen("/dev/tty", "r"); // assert: fopen returns 0  
	int stdout = fopen("/dev/tty", "w"); // assert: fopen returns 1   
	int stderr = fopen("/dev/tty", "w"); // assert: fopen returns 2 

    // TODO: Set up the file system
    
    for (i=0;i<MAX_ENVIRON;i++)
        __otter_environ[i] = symbolic_string(MAX_ENVVAR_LENGTH + 1 + MAX_ENVVAL_LENGTH);  // "name=value"
    __otter_environ[MAX_ENVIRON] = 0;
    environ = &__otter_environ[0];

    return main(argc, argv);
}
