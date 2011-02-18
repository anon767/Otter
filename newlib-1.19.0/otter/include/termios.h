#ifndef _TERMIOS_H
#define _TERMIOS_H

#include <sys/termios.h>

/* Otter specific mocking */
#define _TERMIOS_MAX_FD 2     
struct { struct termios mode; int has_set; } _TERMIOS_TABLE[_TERMIOS_MAX_FD];

int tcgetattr(int fildes, struct termios *termios_p);
int tcsetattr(int fildes, int optional_actions, const struct termios *termios_p);

#endif
