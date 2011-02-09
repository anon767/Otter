#include <inttypes.h>
#include <stdlib.h>

uintmax_t strtoumax(const char * restrict nptr, char ** restrict endptr, int base) {
    return strtoul(nptr, endptr, base);
}
