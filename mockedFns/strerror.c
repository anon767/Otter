#include <string.h>

static char error[] = {'s', 't', 'r', 'e', 'r', 'r', 'o', 'r', ' ', 'm', 'e', 's', 's', 'a', 'g', 'e'};

char *strerror(int errnum)
{
  char *msg = malloc(sizeof(char) * strlen(error));

  strcpy(msg, error);

  return msg;
}
