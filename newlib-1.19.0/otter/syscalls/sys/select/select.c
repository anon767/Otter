#include "otter/otter_builtins.h"

#include <sys/select.h>

int select(int nfds, 
           fd_set *restrict readfds, 
           fd_set *restrict writefds, 
           fd_set *restrict errorfds, 
           struct timeval *restrict timeout) {

    if (readfds == NULL && writefds == NULL && errorfds == NULL) {
        // TODO: sleep for (timeout) secs.
        return 0;
    }

    __ASSERT(0); // Crash
}

