#include "otter/otter_builtins.h"

#include <unistd.h>

long gethostid(void) {
    int id; __SYMBOLIC(&id);
    return id;
}

