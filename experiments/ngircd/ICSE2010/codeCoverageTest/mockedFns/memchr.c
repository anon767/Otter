#include <string.h>

void *memchr(const void *s, int c, size_t n)
{
  char *r = (const char *) s;

  while (n) {
    if (*r == ((char)c)) {
      return (void *) r;	/* silence the warning */
    }
    ++r;
    --n;
  }

  return NULL;
}
