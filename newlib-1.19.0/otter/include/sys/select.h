/* Copied from http://www.koders.com/c/fid53A7AC4B5D3463A7C0A7FF6E837BA26E9AFB4316.aspx?s=cdefs.h */

#ifndef _SYS_SELECT_H
#define _SYS_SELECT_H

#include <string.h>

typedef long int fd_mask;

#define NFDBITS	(8 * sizeof(unsigned long))
#define FD_SETSIZE	1024
#define __FDSET_LONGS	(FD_SETSIZE/NFDBITS)
#define __FDELT(d)	((d) / NFDBITS)
#define __FDMASK(d)	(1UL << ((d) % NFDBITS))

typedef struct {
  unsigned long fds_bits [__FDSET_LONGS];
} fd_set;

#define FD_SET(d, set)	((set)->fds_bits[__FDELT(d)] |= __FDMASK(d))
#define FD_CLR(d, set)	((set)->fds_bits[__FDELT(d)] &= ~__FDMASK(d))
#define FD_ISSET(d, set)	(((set)->fds_bits[__FDELT(d)] & __FDMASK(d)) != 0)
#define FD_ZERO(set)	\
  ((void) memset ((void*) (set), 0, sizeof (fd_set)))

int select(int nfds, 
           fd_set *restrict readfds, 
           fd_set *restrict writefds, 
           fd_set *restrict errorfds, 
           struct timeval *restrict timeout);

#endif
