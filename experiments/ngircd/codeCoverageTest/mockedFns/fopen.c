#include <stdio.h>
#include <fcntl.h>
#include "iosim.h"

FILE * fopen (const char * filename, const char * mode)
{
  int fd = open(filename, O_RDONLY, "r");
  return  IOSIM_fd[fd];
}
