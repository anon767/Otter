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

	sym_file_t* sed = IOSIM_addfile("manis.sed", 0);
	sed->contents = "s/%@/@@/; s/@%/@@/; s/%g$/@g/; /@g$/s/[\\\\\\\\&%]/\\\\\\\\&/g; \n s/@@/%@/; s/@@/@%/; s/@g$/%g/";
	sed->stat.st_size = 87;

	sym_file_t* input = IOSIM_addfile("manis.inp", 0);
	input->contents = "s%@CFLAGS@%%g\ns%@CPPFLAGS@%-I/%g\ns%@CXXFLAGS@%-x c++%g\ns%@DEFS@%$DEFS%g\ns%@LDFLAGS@%-L/usr/lib%g\ns%@LIBS@%-lgnu -lbfd%g\ns%@exec_prefix@%%g\ns%@prefix@%$prefix%g\ns%@RANLIB@%$RANLIB%g\ns%@CC@%/usr/local/bin/gcc%g\ns%@CPP@%$CPP%g\ns%@XCFLAGS@%$XCFLAGS%g\ns%@XINCLUDES@%$XINCLUDES%g\ns%@XLIBS@%$XLIBS%g\ns%@XPROGS@%$XPROGS%g\ns%@TCLHDIR@%$TCLHDIR%g\ns%@TCLLIB@%$TCLLIB%g\ns%@TKHDIR@%$TKHDIR%g\ns%@TKLIB@%$TKLIB%g\ns%@PTY_TYPE@%$PTY_TYPE%g\ns%@EVENT_TYPE@%$EVENT_TYPE%g\ns%@SETUID@%$SETUID%g";
	input->stat.st_size = 472;
}