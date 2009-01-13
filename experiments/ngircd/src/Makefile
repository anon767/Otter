# Remember to change the DIRs for your system!
CURDIR=.
CIL_DIR=/Users/kkma/repo/symexe/cil/

VERSION=ngircd-0.12.0
NGIRCD_DIR=$(CURDIR)/$(VERSION)

CILLY_OPT= --merge \
		  -fno-builtin \
		  -fno-inline \
		  --keepmerged \

CILLY= $(CIL_DIR)/bin/cilly $(CILLY_OPT)

CONFIG_OPT= \
		--without-epoll \
		--without-devpoll \
		--without-kqueue \
		--without-syslog \
		--without-zlib \

		#--without-poll \   # Need at least one of these. I chose poll.

merge: conf
	cd $(NGIRCD_DIR) && \
	make AR="$(CILLY) --mode=AR" && \
	mv src/ngircd/ngircd_comb.c ../$(VERSION).c

conf: 
	cd $(NGIRCD_DIR) && \
	CC="$(CILLY)" \
	CPPFLAGS="-I$(CURDIR)/include -D __client_c__ -D __channel_c__ -D _DONT_USE_CTYPE_INLINE_" \
	RANLIB=echo \
	./configure $(CONFIG_OPT)

clean:
	cd $(NGIRCD_DIR) && make clean

