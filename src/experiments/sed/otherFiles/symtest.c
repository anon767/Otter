#include "iosim.h"
#include <stdlib.h>

void symtest_initialize() {

        // Playing fast and loose with pointers here
        // Make files for stdout and stderr
        IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
        IOSIM_fd[1]->offset = 0;
        IOSIM_fd[1]->fd = 1;
        IOSIM_fd[1]->sym_file = malloc(sizeof(sym_file_t));
        IOSIM_fd[1]->sym_file->contents = NULL;
        IOSIM_fd[1]->sym_file->stat.st_size = 0;
        stdout = IOSIM_fd[1];

  sym_file_t* file_0range = IOSIM_addfile("0range.inp", 0);
  file_0range->contents = "1\n2\n3\n4\naaa\nyes";
  file_0range->stat.st_size = 15;

  sym_file_t* file_allsub = IOSIM_addfile("allsub.inp", 0);
  file_allsub->contents = "foo foo fo oo f oo foo foo foo foo foo foo foo foo foo foo foo foo foo";
  file_allsub->stat.st_size = 70;


/*
  sym_file_stream_t *fileS = malloc(sizeof(sym_file_stream_t));
  fileS->sym_file = &fileT;
  fileS->offset = 0;
*/
/*
  IO_BUF* stdin = IOSIM_newbuf(-1, "-e s/haha/hehe/ test.txt");
  IOSIM_attach(0, stdin);

  IO_BUF* sed_exp = IOSIM_newbuf(-1, "1haha1\n2haha2\n3haha3");
  IOSIM_addfile("test.txt", sed_exp);
*/
}

