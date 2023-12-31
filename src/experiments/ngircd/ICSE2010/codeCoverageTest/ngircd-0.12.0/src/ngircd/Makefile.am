#
# ngIRCd -- The Next Generation IRC Daemon
# Copyright (c)2001-2003 by Alexander Barton (alex@barton.de)
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# Please read the file COPYING, README and AUTHORS for more information.
#
# $Id: Makefile.am,v 1.51 2008/02/26 22:04:17 fw Exp $
#

AUTOMAKE_OPTIONS = ../portab/ansi2knr

INCLUDES = -I$(srcdir)/../portab -I$(srcdir)/../tool -I$(srcdir)/../ipaddr

LINTARGS = -weak -warnunixlib +unixlib -booltype BOOLEAN \
 -varuse -retvalother -emptyret -unrecog

sbin_PROGRAMS = ngircd

ngircd_SOURCES = ngircd.c array.c channel.c client.c conf.c conn.c conn-func.c \
	conn-zip.c hash.c io.c irc.c irc-channel.c irc-info.c irc-login.c \
	irc-mode.c irc-op.c irc-oper.c irc-server.c irc-write.c lists.c log.c \
	match.c numeric.c parse.c rendezvous.c resolve.c

ngircd_LDFLAGS = -L../portab -L../tool -L../ipaddr

ngircd_LDADD = -lngportab -lngtool -lngipaddr

noinst_HEADERS = ngircd.h array.h channel.h client.h conf.h conn.h conn-func.h \
	conn-zip.h hash.h io.h irc.h irc-channel.h irc-info.h irc-login.h \
	irc-mode.h irc-op.h irc-oper.h irc-server.h irc-write.h lists.h log.h \
	match.h numeric.h parse.h rendezvous.h resolve.h \
	defines.h messages.h

clean-local:
	rm -f check-version check-help lint.out

maintainer-clean-local:
	rm -f Makefile Makefile.in

check-version: Makefile
	echo "#!/bin/sh" > check-version
	echo "./ngircd --version | grep ngircd >/dev/null 2>&1" >>check-version
	chmod 755 check-version

check-help: Makefile
	echo "#!/bin/sh" > check-help
	echo "./ngircd --help | grep help >/dev/null 2>&1" >>check-help
	chmod 755 check-help

lint:
	@splint --version >/dev/null 2>&1 \
	 || ( echo; echo "Error: \"splint\" not found!"; echo; exit 1 )
	@echo; warnings=0; files=0; \
	for f in *.c; do \
	 echo "checking $$f ..."; \
	 splint $$f $(LINTARGS) -I$(srcdir) -I$(srcdir)/.. \
	  $(INCLUDES) $(AM_CFLAGS) >lint.out 2>&1; \
	 grep "no warnings" lint.out > /dev/null 2>&1; \
	 if [ $$? -ne 0 ]; then \
	  waswarning=1; \
	  echo; grep -v "^Command Line: " lint.out; echo; \
	  w=$$( grep "code warning" lint.out | $(AWK) "{ print \$$4 }" ); \
	  [ "$$w" -gt 0 ] && warnings=`expr $$warnings + $$w`; \
	  files=`expr $$files + 1`; \
	 else \
	  waswarning=0; \
	 fi; \
	 rm -f lint.out; \
	done; \
	[ $$waswarning -eq 0 ] && echo; \
	[ $$warnings -gt 0 ] \
	 && echo "Result: $$warnings warning(s) in $$files file(s)!" \
	 || echo "Result: no warnings found."; \
	echo; [ $$warnings -gt 0 ] && exit 1

TESTS = check-version check-help

# -eof-
