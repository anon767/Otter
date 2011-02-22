/*
It's hard to find good documentation saying exactly what select is supposed to
do. The full SUSv3 spec is hard to find online, but it is at
http://www.unix.org/single_unix_specification_v3/ and does have a good
description.

Here are a few easier-to-find pieces of documentation that I found.
http://linux.die.net/man/2/select says:

	Three independent sets of file descriptors are watched. Those listed in
	readfds will be watched to see if characters become available for reading
	(more precisely, to see if a read will not block; in particular, a file
	descriptor is also ready on end-of-file), those in writefds will be watched to
	see if a write will not block, and those in exceptfds will be watched for
	exceptions. On exit, the sets are modified in place to indicate which file
	descriptors actually changed status. Each of the three file descriptor sets
	may be specified as NULL if no file descriptors are to be watched for the
	corresponding class of events.

Additionally, opengroup says:

	File descriptors associated with regular files always select true for ready to
	read, ready to write, and error conditions.

I'm interpreting this to include /dev/null, /dev/zero, and /dev/tty, as well.
And directories, too, although I'm not sure that makes sense.

And also, http://www.lowtek.com/sockets/select.html says that a socket in
	'listen' mode is considered 'ready for reading' if someone has 'connect'ed to
	it:

	If a client is trying to connect() to our listening socket, select() will
	consider that as the socket being 'readable'.

Since Otter doesn't have signals, the errorfds argument is all but useless:
there is no such thing as being 'ready for error'. I guess we still have to test
to see if any of these is a regular file, because of what opengroup says, but
that's it. For now, we just bail if anyone specifies any errorfds.

Another note: Otter's sense of time is quite limited. For now, we completely
ignore the time parameter to select, and simply wait for one of the fds to be
ready. The only exception is if all of the fd_sets are NULL or empty, then we
sleep for the specified amount of time. */

#include <otter/otter_fs.h>
#include <otter/otter_builtins.h>
#include <otter/otter_scheduler.h>

#include <stddef.h> // For NULL
#include <stdlib.h> // For malloc
#include <string.h> // For memcpy
#include <errno.h>
#include <sys/select.h>
#include <sys/socket.h>

/* Checks whether an open file can be read without blocking. If a read would
	 block, this function returns a pointer to memory that will change when a read
	 can succeed on this file. Otherwise---if a read would not block---this
	 function returns NULL.

	 select will eventually do an io_block on all such returned pointers if no
	 file is ready. */
static void *ready_to_read(struct __otter_fs_open_file_table_entry *open_file) {
	if (open_file->type == __otter_fs_TYP_SOCK) {
		struct __otter_fs_sock_data *sock = __otter_libc_get_sock_data_from_open_file(open_file);
		switch (sock->state) {
		// A listening socket is ready to read if someone called connect---that is, if there is someone in the sock_queue.
		case __otter_sock_ST_LISTEN:
			if (sock->sock_queue[0] != NULL) {
				return NULL;
			} else {
				// The socket becomes ready for reading when someone enters the sock_queue, so monitor that
				return sock->sock_queue;
			}
		// An established socket is ready to read if there is data in its recv_data
		case __otter_sock_ST_ESTABLISHED:
		case __otter_sock_ST_FIN_WAIT_1:
		case __otter_sock_ST_FIN_WAIT_2:
			if (!__otter_fs_pipe_is_empty(sock->recv_data)) {
				return NULL;
			} else {
				// The pipe is not ready for reading. Watch its write head, which will move when it's ready for reading.
				return &sock->recv_data->whead;
			}
		// Any other state will lead to a failing read, but it will not block, so it is ready
		case __otter_sock_ST_CLOSED:
		case __otter_sock_ST_SYN_RCVD:
		case __otter_sock_ST_SYN_SENT:
		case __otter_sock_ST_CLOSE_WAIT:
		case __otter_sock_ST_LAST_ACK:
		case __otter_sock_ST_CLOSING:
		case __otter_sock_ST_TIME_WAIT:
		case __otter_sock_ST_UDP:
			return NULL;
		}
		__ASSERT(0); // Unreachable
		abort(); // To make CIL happy
	} else if (open_file->type == __otter_fs_TYP_FIFO) {
		struct __otter_fs_pipe_data *pipe = __otter_libc_get_pipe_data_from_open_file(open_file);
		if (!__otter_fs_pipe_is_empty(pipe)) {
			// The pipe is ready for reading
			return NULL;
		} else {
			// The pipe is not ready for reading. Watch its write head, which will move when it's ready for reading.
			return &pipe->whead;
		}
	} else { // Everything else is always ready for reading
		return NULL;
	}
}

/* Similar to ready_to_read, returns NULL if a write would *not* block.
	 Otherwise, returns a pointer to memory that will change when a write can be
	 done. */
static void *ready_to_write(struct __otter_fs_open_file_table_entry *open_file) {
	if (open_file->type == __otter_fs_TYP_SOCK) {
		struct __otter_fs_sock_data *sock = __otter_libc_get_sock_data_from_open_file(open_file);
		if (sock->state == __otter_sock_ST_ESTABLISHED) {
			if (!__otter_fs_pipe_is_full(sock->sock_queue[0]->recv_data)) {
				return NULL; // A socket is ready for writing if it is established and there is room in its write buffer
			} else {
				// If a socket is established but empty, monitor its write buffer's read head. It is ready for writing when the read head moves.
				return &sock->sock_queue[0]->recv_data->rhead;
			}
		} else {
			// The socket is in the wrong state. Monitor the state to see if it becomes ready.
			return &sock->state;
		}
	} else if (open_file->type == __otter_fs_TYP_FIFO) {
		struct __otter_fs_pipe_data *pipe = __otter_libc_get_pipe_data_from_open_file(open_file);
		if (__otter_fs_pipe_is_full(pipe)) {
			// Monitor the pipe's read head; the pipe is ready for writing when the read head moves.
			return &pipe->rhead;
		} else { // The pipe is not full, so it's ready for writing.
			return NULL;
		}
	} else { // Everything else is always ready for writing
		return NULL;
	}	
}

/* Tests if a file descriptor bit is set, taking into account the fact that the
	 fd_set pointer might be null. */
static int is_set(int fd, fd_set *fd_set_p) {
	return fd_set_p && FD_ISSET(fd, fd_set_p);
}

/* A macro to check whether a file descriptor is ready. This refers to local
	variables within select_helper's 'for' loop and can only be used there. */
#define check_for_readiness(fds, ready_check_fn)\
	/* See if the pointer is ready for reading */\
	if (is_set(fd, (fds))) {\
		void *ptr = (ready_check_fn)(open_file);\
		if (ptr) {\
			/* retval is non-null, so the file is not ready. */\
			FD_CLR(fd, (fds));\
			watch_list[watch_list_len] = ptr; /* Record the pointer to watch */\
			watch_list_len++; /* And increment the index into watch_list */\
		} else {\
			num_ready_fds++; /* The file is ready */\
		}\
	}

// Sentinel value to indicate that we had to block on the requested conditions.
#define SELECT_HELPER_BLOCKED (-2)

/* Helper that does the real work for select. It returns > 0 if any fds were
	 ready (and sets the fd_sets accordingly), -1 on error, and
	 SELECT_HELPER_BLOCKED if nothing was ready, so it blocked. If it blocks, it
	 zeros out all fd_sets, so they will probably need to be restored afterward. */
static int select_helper(int nfds, fd_set *readfds, fd_set *writefds, fd_set *errorfds) {
	int num_ready_fds = 0;

	/* This will hold all of the pointers that we will block on if nothing is
		 ready now. The length of the array is nfds*3 because each of the fd_sets
		 might give us a different pointer to watch. */
	void **watch_list = malloc(sizeof(void*) * nfds * 3);
	int watch_list_len = 0;

	for (int fd = 0; fd < nfds; fd++) {
		if (!(is_set(fd, readfds) || is_set(fd, writefds))) {
			continue;
		}
		if (is_set(fd, errorfds)) {
			__EVALSTR("The current implementation of select doesn't handle errorfds", 100);
			__ASSERT(0);
		}

		struct __otter_fs_open_file_table_entry *open_file = get_open_file_from_fd(fd);
		if (!open_file) {
			errno = EBADF;
			free(watch_list);
			return -1;
		}

		check_for_readiness(readfds, ready_to_read);
		check_for_readiness(writefds, ready_to_write);
//		check_for_readiness(errorfds, has_pending_error);
	}

	// If anything is ready, return.
	if (num_ready_fds) {
		free(watch_list);
		return num_ready_fds;
	}

	// If nothing is ready, wait on the specified file descriptors.
	if (watch_list_len == 1) {
		__otter_multi_io_block(watch_list[0]);
	} else {
		__EVALSTR("select can currently only block on a single event", 50);
		__ASSERT(0);
	}
	free(watch_list);
	return SELECT_HELPER_BLOCKED;
}

/* Copies an fd_set, but only if both src and dest are non-null. */
static void copy_fd_set(fd_set *dest, fd_set *src) {
	if (src && dest) {
		memcpy(dest, src, sizeof(fd_set));
	}
}

int select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *errorfds,
					 struct timeval *restrict timeout) {
	if (nfds < 0 || nfds >= FD_SETSIZE) {
		errno = EINVAL;
		return -1;
	}

	if (nfds == 0) { // The caller is using select as sleep
		struct timespec t = {.tv_sec = timeout->tv_sec, .tv_nsec = timeout->tv_usec * 1000};
		nanosleep(&t, NULL);
		return 0;
	}

	// We will copy the fd_sets so that they remain unchanged in case of an error
	fd_set *read_copy = readfds ? malloc(sizeof(fd_set)) : NULL;
	fd_set *write_copy = writefds ? malloc(sizeof(fd_set)) : NULL;
	fd_set *error_copy = errorfds ? malloc(sizeof(fd_set)) : NULL;

	int retval;
	/* Until one of the selected-for conditions is true (which is indicated by
		 select_helper return a value other than SELECT_HELPER_BLOCKED), do the
		 following:
		 1. Copy the original fd_sets into the *_copy variables
		 2. Call select_helper */
	do {
		copy_fd_set(read_copy, readfds);
		copy_fd_set(write_copy, writefds);
		copy_fd_set(error_copy, errorfds);
		retval = select_helper(nfds, read_copy, write_copy, error_copy);
	} while (retval == SELECT_HELPER_BLOCKED);

	/* If there was an error, the original fd_sets should *not* be modified. So
		 copy the modified fd_sets to the inputs only if there was *no* error. */
	if (retval != -1) {
		copy_fd_set(readfds, read_copy);
		copy_fd_set(writefds, write_copy);
		copy_fd_set(errorfds, error_copy);
	}
	// Free the copies. Some might be null, but free(NULL) is a no-op.
	free(read_copy);
	free(write_copy);
	free(error_copy);
	return retval;
}
