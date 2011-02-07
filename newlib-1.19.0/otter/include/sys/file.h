#ifndef	_SYS_FILE_H
#define	_SYS_FILE_H

/* adapted from uclibc */

/* Copyright (C) 1991, 92, 96, 97, 98, 99 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307 USA.  */

#include <fcntl.h>

/* Alternate names for values for the WHENCE argument to `lseek'.
   These are the same as SEEK_SET, SEEK_CUR, and SEEK_END, respectively.  */
#ifndef L_SET
# define L_SET	0	/* Seek from beginning of file.  */
# define L_INCR	1	/* Seek from current position.  */
# define L_XTND	2	/* Seek from end of file.  */
#endif


/* Operations for the `flock' call.  */
#define	LOCK_SH	1	/* Shared lock.  */
#define	LOCK_EX	2 	/* Exclusive lock.  */
#define	LOCK_UN	8	/* Unlock.  */

/* Can be OR'd in to one of the above.  */
#define	LOCK_NB	4	/* Don't block when locking.  */


/* Apply or remove an advisory lock, according to OPERATION,
   on the file FD refers to.  */
int flock(int __fd, int __operation);

#endif

