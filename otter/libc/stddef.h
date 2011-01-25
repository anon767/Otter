#ifndef _STDDEF_H
#define _STDDEF_H

#include <__otter/config.h>

#define NULL ((void *)0)
#define offsetof(x, y) offsetof(x, y)

typedef TYPE_SIZE_T size_t;
typedef TYPE_WCHAR_T wchar_t;
typedef TYPE_PTRDIFF_T ptrdiff_t;

#endif
