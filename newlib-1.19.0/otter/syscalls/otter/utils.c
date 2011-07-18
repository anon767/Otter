#include "otter/utils.h"
#include <string.h>

/* Similar to strchr/strrchr, but these functions returnt the integer indices of
 * the character (or -1 if not found), instead of char pointers.
 * Use these to avoid pointer minus arithmetic.
 */
int strchr_i (const char *s, int c) {
    int i;
    for(i=0;s[i]!=0;i++)
        if (s[i]==c) return i;
    return -1;
}

int strrchr_i (const char *s, int c) {
    int i;
    for(i=strlen(s)-1;i>=0;i--)
        if (s[i]==c) return i;
    return -1;
}
