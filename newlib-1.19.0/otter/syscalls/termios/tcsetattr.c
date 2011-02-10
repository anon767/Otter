#include "otter/utils.h"

#include <termios.h>
#include <errno.h>

/* FIXME: action is ignored for now */
int tcsetattr(int fd, int action, const struct termios *t) {

    __OTTER_POSSIBILY_FAILING_SYSCALL_BEGIN__

    _TERMIOS_TABLE[fd].mode = *t;
    _TERMIOS_TABLE[fd].has_set = 1;
    return 0;

    __OTTER_POSSIBILY_FAILING_SYSCALL_END__
}
