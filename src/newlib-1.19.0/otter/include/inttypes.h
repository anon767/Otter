#ifndef _INTTYPES_H
#define _INTTYPES_H

#include <stdint.h>

intmax_t  strtoimax(const char * restrict nptr, char ** restrict endptr, int base);
uintmax_t strtoumax(const char * restrict nptr, char ** restrict endptr, int base);

#endif
