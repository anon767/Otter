# Define the following:
CC = ../bin/otter-libc-cc
AR = ../../../cil/bin/cilly --merge --mode=AR
RANLIB = ../bin/otter-ranlib

OBJS = sys/time/_gettimeofday_r.o \
	   otter/otter_fs.o \
	   otter/otter_fs_util.o

INCLUDES = -I./include
ARFLAGS = cru

all: libottersys.a

.c.o:
	$(CC) $(INCLUDES) -c $< -o $@

libottersys.a: $(OBJS)
	${AR} ${ARFLAGS} $@ $(OBJS)
	${RANLIB} $@

clean :
	find . -name "*.o" -exec rm {} \;

distclean: clean
	rm -f libottersys.a

