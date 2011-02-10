#include <termios.h>
#include <errno.h>

/* FIXME: action is ignored for now */
int tcsetattr(int fd, int action, const struct termios *t) {
    int success;
    __SYMBOLIC(&success);
    if (success) {
        _TERMIOS_TABLE[fd].mode = *t;
        _TERMIOS_TABLE[fd].has_set = 1;
        return 0;
    } else {
        __SYMBOLIC(&errno);
        return -1;
    }
}
