# Remember to change the DIRs for your system!

VERSION=ngircd-0.12.0
NGIRCD_DIR=./$(VERSION)

CILLY_OPT= --merge \
		  -fno-builtin \
		  -fno-inline \
		  --keepmerged \

CILLY:= $(CURDIR)/../../../cil/bin/cilly $(CILLY_OPT)
LIBC:= $(CURDIR)/../../../libc

CONFIG_OPT= \
		--without-epoll \
		--without-devpoll \
		--without-kqueue \
		--without-syslog \
		--without-zlib \
		#--without-poll \   # Need at least one of these. I chose poll.

merge: 
	make -C $(NGIRCD_DIR) AR="$(CILLY) --mode=AR"
	cp $(NGIRCD_DIR)/src/ngircd/ngircd_comb.c ngircd.c

conf: 
	cd $(NGIRCD_DIR) && \
	CC="$(CILLY)" \
	CPPFLAGS="-I $(LIBC) -D __client_c__ -D __channel_c__ -D _DONT_USE_CTYPE_INLINE_ -D __NO_INLINE__ -U __OPTIMIZE__" \
	RANLIB=echo \
	./configure $(CONFIG_OPT)

clean:
	cd $(NGIRCD_DIR) && make clean 

