#include <stdio.h>
#include <fcntl.h>

FILE * fopen (const char * filename, const char * mode)
{
  return open(filename, O_RDONLY, "r");
}
