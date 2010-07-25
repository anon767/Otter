#ifndef _STDARG_H
#define _STDARG_H

#define NULL 0
#define offsetof(x, y) offsetof(x, y)

/* This should work but doesn't for some reason
typedef unsigned int size_t;
typedef int wchar_t;
typedef int ptrdiff_t;
*/

#define size_t unsigned int
#define wchar_t int
#define ptrdiff_t int

#endif
