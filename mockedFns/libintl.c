#include <libintl.h>
#include <string.h>

char * directory;

char * bindtextdomain (const char * domainname, const char * dirname)
{
  directory = malloc(1028*4);
  strcpy(directory, dirname);

  return domainname;
}

char * textdomain (const char * domainname)
{
  return directory;
}
