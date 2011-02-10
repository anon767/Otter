#include <unistd.h>

int isatty(int fd) {
    return ttyname(fd) != NULL;
}

