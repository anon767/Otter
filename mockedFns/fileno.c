#include "iosim.h"
#include <stdio.h>

int fileno(register FILE *stream)
{
        int retval;

        retval = fileno_unlocked(stream);

        return retval;
}


int fileno_unlocked(register FILE *stream)
{
	// Cheat by treating this as the right kind of pointer
	sym_file_stream_t *s = (sym_file_stream_t *)stream;
        if (s->fd >= 0) {
                return s->fd;
        }

        return -1;
}

