#ifndef _STDDEF_H
#define _STDDEF_H

typedef unsigned int size_t;

/* Is this okay? */
typedef short wchar_t;

/* The spec says this should be defined in <wchar.h>, but newlib's
 * <wchar.h> expects it here. Put it here to make things work. */
typedef int wint_t;

/* If pointers are unsigned ints, ptrdiff_t needs to be big enough to
	 hold values up to the maximum unsigned int. But ptrdiff_t also
	 needs to be signed, so it actually needs to be long long; int isn't
	 big enough. */
typedef long long ptrdiff_t;

#define NULL ((void *)0)

/* I got this definition from http://en.wikipedia.org/wiki/Offsetof on Jan 13, 2011 */
#define offsetof(st, m) ((size_t) ( (char *)&((st *)(0))->m - (char *)0 ))

#endif
