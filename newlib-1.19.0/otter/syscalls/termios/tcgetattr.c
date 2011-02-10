#include <termios.h>
#include <errno.h>

int tcgetattr(int fd, struct termios *t) {
    int success;
    __SYMBOLIC(&success);
    if (success) {
        if (!_TERMIOS_TABLE[fd].has_set) {
            struct termios mode;
            __SYMBOLIC(&mode);
            _TERMIOS_TABLE[fd].mode = mode;
        }
        *t = _TERMIOS_TABLE[fd].mode;
        return 0;
    } else {
        __SYMBOLIC(&errno);
        return -1;
    }
}
