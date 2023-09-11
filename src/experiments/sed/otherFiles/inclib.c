#include "iosim.h"
#include <stdlib.h>

void symtest_initialize() {
	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->offset = 0;
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_file->contents = NULL;
	IOSIM_fd[1]->sym_file->stat.st_size = 0;
	stdout = IOSIM_fd[1];

	sym_file_t* sed = IOSIM_addfile("inclib.sed", 0);
	sed->contents = "s;lib;include;";
	sed->stat.st_size = 14;

	sym_file_t* input = IOSIM_addfile("inclib.inp", 0);
	input->contents = "	/usr/X11R6/lib        \n	/usr/X11R5/lib        \n	/usr/X11R4/lib        \n	\n	/usr/lib/X11R6        \n	/usr/lib/X11R5        \n	/usr/lib/X11R4        \n	\n	/usr/local/X11R6/lib  \n	/usr/local/X11R5/lib  \n	/usr/local/X11R4/lib  \n	\n	/usr/local/lib/X11R6  \n	/usr/local/lib/X11R5  \n	/usr/local/lib/X11R4  \n	\n	/usr/X11/lib          \n	/usr/lib/X11          \n	/usr/local/X11/lib    \n	/usr/local/lib/X11    \n	\n	/usr/X386/lib         \n	/usr/x386/lib         \n	/usr/XFree86/lib/X11  \n	\n	/usr/lib              \n	/usr/local/lib        \n	/usr/unsupported/lib  \n	/usr/athena/lib       \n	/usr/local/x11r5/lib  \n	/usr/lpp/Xamples/lib  \n	\n	/usr/openwin/lib      \n	/usr/openwin/share/lib ";
	input->stat.st_size = 662;
}