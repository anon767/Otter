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

	sym_file_t* sed = IOSIM_addfile("xemacs.sed", 0);
	sed->contents = "\n/^# Generated/d\ns%/\\*\\*/#.*%%\ns/^ *# */#/\n/^##/d\n/^#/ {\n  p\n  d\n}\n/./ {\n  s/\\([\\"]\\)/\\\\\\1/g\n  s/^/"/\n  s/$/"/\n}";
	sed->stat.st_size = 112;

	sym_file_t* input = IOSIM_addfile("xemacs.inp", 0);
	input->contents = "\n\n\n@SET_MAKE@\n\n\nSHELL = @SHELL@\n\nPACKAGE = sed\n\nEXTRA_DIST = BUGS THANKS README.boot bootstrap.sh dc.sed autogen \\\n	m4/codeset.m4  m4/gettext.m4  m4/iconv.m4      m4/lcmessage.m4 \\\n	m4/getline.m4  m4/glibc21.m4  m4/isc-posix.m4  m4/progtest.m4 \\\n	m4/obstack.m4\n\nsubdir = .\nACLOCAL_M4 = $(top_srcdir)/aclocal.m4\nmkinstalldirs = $(SHELL) $(top_srcdir)/mkinstalldirs\nCONFIG_HEADER = config.h\nCONFIG_CLEAN_FILES = bootstrap.sh intl/Makefile\nDIST_SOURCES =\nDATA = $(noinst_DATA)\n\nHEADERS = $(noinst_HEADERS)\n\n\nRECURSIVE_TARGETS = info-recursive dvi-recursive install-info-recursive \\\n	uninstall-info-recursive all-recursive install-data-recursive \\\n	install-exec-recursive installdirs-recursive install-recursive \\\n	uninstall-recursive check-recursive installcheck-recursive\nDIST_COMMON = README $(noinst_HEADERS) ./stamp-h.in ABOUT-NLS AUTHORS \\\n	COPYING ChangeLog INSTALL Makefile.am Makefile.in NEWS THANKS \\\n	TODO acconfig.h aclocal.m4 bootstrap.sh.in config.guess \\\n	config.sub config_h.in configure configure.ac depcomp \\\n	install-sh missing mkinstalldirs\nDIST_SUBDIRS = $(SUBDIRS)\nall: config.h\n	$(MAKE) $(AM_MAKEFLAGS) all-recursive\n\n.SUFFIXES:\n$(srcdir)/Makefile.in:  Makefile.am  $(top_srcdir)/configure.ac $(ACLOCAL_M4)\n	cd $(top_srcdir) && \\\n	  $(AUTOMAKE) --gnu  Makefile\nMakefile:  $(srcdir)/Makefile.in  $(top_builddir)/config.status\n	cd $(top_builddir) && \\\n	  CONFIG_HEADERS= CONFIG_LINKS= \\\n	  CONFIG_FILES=$@ $(SHELL) ./config.status\n\n$(top_builddir)/config.status: $(srcdir)/configure $(CONFIG_STATUS_DEPENDENCIES)\n	$(SHELL) ./config.status --recheck\n$(srcdir)/configure:  $(srcdir)/configure.ac $(ACLOCAL_M4) $(CONFIGURE_DEPENDENCIES)\n	cd $(srcdir) && $(AUTOCONF)\n\n$(ACLOCAL_M4):  configure.ac m4/codeset.m4 m4/getline.m4 m4/gettext.m4 m4/glibc21.m4 m4/iconv.m4 m4/isc-posix.m4 m4/lcmessage.m4 m4/obstack.m4 m4/progtest.m4\n	cd $(srcdir) && $(ACLOCAL) $(ACLOCAL_AMFLAGS)\nconfig.h: stamp-h";
	input->stat.st_size = 1903;
}