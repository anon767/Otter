#include "otter/otter_builtins.h"
#include "otter/utils.h"
#include "otter/otter_fs.h"
#include "otter/otter_user.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>

extern int main(int argc, char **argv);

// Customized mkroot which has no "." and ".."
struct __otter_fs_dnode* my_otter_fs_mkroot()
{
    struct __otter_fs_dnode* newdir = __otter_multi_gmalloc(sizeof(struct __otter_fs_dnode));
    (*newdir).linkno = 1;
    (*newdir).numfiles = 0;
    (*newdir).numdirs = 0;
    (*newdir).files = NULL;
    (*newdir).dirs = NULL;
    (*newdir).permissions = 0x31ED;

    return newdir;
}

// Customized fs setup which has no stdio and no pwd
void __otter_main_setup_fs() {
    setuid(__otter_UID_ROOT);

    // Setup root
    struct __otter_fs_dnode* root = my_otter_fs_mkroot();
    __otter_fs_root = root;
    (*root).permissions = 0x01ED;

#define MAX_SIZE  8
    // Setup a file named "t" in root which has max size MAX_SIZE
    {
        int size; // = MAX_SIZE;
        __ASSUME(0<=size);
        __ASSUME(size<=MAX_SIZE);
        char* s = symbolic_string(MAX_SIZE);
        __otter_fs_touch_with_data("t", __otter_fs_root, s, size);
    }
    // Setup a file named "u" in root which has max size MAX_SIZE
    {
        int size; // = MAX_SIZE;
        __ASSUME(0<=size);
        __ASSUME(size<=MAX_SIZE);
        char* s = symbolic_string(MAX_SIZE);
        __otter_fs_touch_with_data("u", __otter_fs_root, s, size);
    }

    struct __otter_fs_dnode* dev = __otter_fs_root; //__otter_fs_mkdir("dev", root);
    struct __otter_fs_inode* tty = __otter_fs_touch("s", dev);

    (*tty).permissions = 0x01B6;
    (*tty).type = __otter_fs_TYP_TTY;
    (*dev).permissions = 0x01FF;

    /* mark all file descriptors and file table entries as unused */
    __otter_fs_fd_table = malloc(sizeof(int)*__otter_fs_MAX_FDS); /* local */
    memset(__otter_fs_fd_table, -1, __otter_fs_MAX_FDS*sizeof(int));
    __otter_fs_open_file_table = __otter_multi_gcalloc(sizeof(struct __otter_fs_open_file_table_entry), __otter_fs_MAX_OPEN_FILES);

    stdin  = fopen("/s", "r"); // assert: fopen returns 0  
    __ASSERT(fileno(stdin)==0);
    //stdout = fopen("/s", "w"); // assert: fopen returns 1   
    //__ASSERT(fileno(stdout)==1);
    //stderr = fopen("/s", "w"); // assert: fopen returns 2 
    //__ASSERT(fileno(stderr)==2);

    /* open file(s), to make some fd available */
    //open ("/t", O_RDONLY /*| O_BINARY */);                            // OTTERHACK
}

#pragma cilnoremove("__otter_main_driver")
int __otter_main_driver() {

    int i;
    int argc;
    char* argv[MAX_ARGC+1];  // One for null-termination

    // Set up argc
    __SYMBOLIC(&argc);
    __ASSUME(1<=argc);
    __ASSUME(argc<=MAX_ARGC);

    // Set up argv
    int arg_lengths[] = {MAX_ARG_LENGTHS};
    argv[0] = strdup("p");  // name of the program
    for (i=1;i<MAX_ARGC;i++) {
        argv[i] = symbolic_string(arg_lengths[i]);
    }
    // Use i instead of argc since i is concrete
    // (I'm not sure if the null-termination is necessary. Seems not.)
    argv[i] = 0;

    __otter_main_setup_fs();

    return main(argc, argv);
}