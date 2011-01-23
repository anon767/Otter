INCLUDES = -I include

CC = ../bin/otter-libc-cc $(INCLUDES)
AR = ../../../cil/bin/cilly --merge --mode=AR
RANLIB = ../bin/otter-ranlib

OBJS = sys/time/_gettimeofday_r.o \
	   otter/otter_fs.o \
	   otter/otter_fs_util.o

ARFLAGS = cru

all: libottersys.a

libottersys.a: $(OBJS)
	${AR} ${ARFLAGS} $@ $(OBJS)
	${RANLIB} $@

clean :
	rm $(OBJS)

distclean: clean
	rm -f libottersys.a

