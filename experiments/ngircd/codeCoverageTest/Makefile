# Remember to change the DIRs for your system!

VERSION=ngircd-0.12.0
NGIRCD_DIR=./$(VERSION)
TOPDIR=$(CURDIR)/../../..

CILLY_OPT= --merge \
           --disallowDuplication \
	 -fno-builtin \
	 -fno-inline \
	 --keepmerged \

CILLY:= $(TOPDIR)/cil/bin/cilly $(CILLY_OPT) -nostdinc -Werror-implicit-function-declaration -I $(TOPDIR)/includes/usr/include -I $(TOPDIR)/includes/usr/local/gcc-4.0.1/lib/gcc/i686-pc-linux-gnu/4.0.1/include
CILLYAR:= $(TOPDIR)/cil/bin/cilly $(CILLY_OPT) --useLogicalOperators --mode=AR
LIBC:= $(TOPDIR)/libc

CONFIG_OPT= \
		--without-epoll \
		--without-devpoll \
		--without-kqueue \
		--without-syslog \
		--without-zlib \
		#--without-poll \   # Need at least one of these. I chose poll.

merge: 
	-make -C $(NGIRCD_DIR) AR="$(CILLYAR)"
	cp $(NGIRCD_DIR)/src/ngircd/ngircd_comb.c ngircd.c

conf: 
	cd $(NGIRCD_DIR) && \
	CC="$(CILLY) --useLogicalOperators" \
	CPPFLAGS="-I $(LIBC) -D __client_c__ -D __channel_c__ -D _DONT_USE_CTYPE_INLINE_ -D __NO_INLINE__ -U __OPTIMIZE__" \
	RANLIB=echo \
	./configure $(CONFIG_OPT)

merge_unfolded: 
	make -C $(NGIRCD_DIR) AR="$(CILLY) --mode=AR"
	cp $(NGIRCD_DIR)/src/ngircd/ngircd_comb.c ngircd_unfolded.c

conf_unfolded: 
	cd $(NGIRCD_DIR) && \
	CC="$(CILLY)" \
	CPPFLAGS="-I $(LIBC) -D __client_c__ -D __channel_c__ -D _DONT_USE_CTYPE_INLINE_ -D __NO_INLINE__ -U __OPTIMIZE__" \
	RANLIB=echo \
	./configure $(CONFIG_OPT)

clean:
	cd $(NGIRCD_DIR) && make distclean

