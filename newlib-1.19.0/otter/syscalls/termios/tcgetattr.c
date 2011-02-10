#include "otter/otter_builtins.h"
#include "otter/utils.h"

#include <termios.h>
#include <errno.h>

int tcgetattr(int fd, struct termios *t) {

    __OTTER_POSSIBILY_FAILING_SYSCALL_BEGIN__

    if (!_TERMIOS_TABLE[fd].has_set) {
        struct termios mode;
        __SYMBOLIC(&mode);
        _TERMIOS_TABLE[fd].mode = mode;
    }
    *t = _TERMIOS_TABLE[fd].mode;
    return 0;

    __OTTER_POSSIBILY_FAILING_SYSCALL_END__
}
