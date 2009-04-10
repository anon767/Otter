include ../Makefile.common

SRCS = simplifier.cpp bvsolver.cpp
OBJS = $(SRCS:.cpp=.o)

libsimplifier.a: $(OBJS)
	$(RM) $@
	$(AR) rc $@ $^
	$(RANLIB) $@

clean:	
	rm -rf *.o *~ *.a .#*
