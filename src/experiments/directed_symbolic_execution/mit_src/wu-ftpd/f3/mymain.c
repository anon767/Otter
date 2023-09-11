#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>

#ifdef CIL
#include "otter/otter_builtins.h"
#include <string.h>

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
#endif

int myfoo(int argc, char **argv);

int main() {
  char* argv[3];
  char* name = "myfoo";
  size_t dummy;

#ifdef CIL
  argv[0] = name;
  argv[1] = symbolic_string(40);
  argv[2] = symbolic_string(40);
  //argv[1] = strdup("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  //argv[2] = strdup("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
#else
  FILE * fp = fopen ("testcase", "r");
  if (!fp) {
	fprintf (stderr, "Could not find a testfile\n");
	exit(1);  
  }
  argv[0] = name;
  argv[1] = NULL;
  argv[2] = NULL;

  getline(&(argv[1]), &dummy, fp);
  getline(&(argv[2]), &dummy, fp);
#endif

  return (myfoo(3, argv));
}
