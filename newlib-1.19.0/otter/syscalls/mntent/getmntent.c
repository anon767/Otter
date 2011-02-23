#include "otter/otter_builtins.h"

#include <stdio.h>
#include <mntent.h>

FILE *setmntent(const char *filename, const char *type) {
    __ASSERT(0);
    return 0;
}

struct mntent *getmntent(FILE *fp) {
    __ASSERT(0);
    return 0;
}

int addmntent(FILE *fp, const struct mntent *mnt) {
    __ASSERT(0);
    return 0;
}

int endmntent(FILE *fp) {
    __ASSERT(0);
    return 0;
}

char *hasmntopt(const struct mntent *mnt, const char *opt) {
    __ASSERT(0);
    return 0;
}
