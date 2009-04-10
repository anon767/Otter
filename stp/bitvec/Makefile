include ../Makefile.common

SRCS = consteval.cpp
OBJS = $(SRCS:.cpp=.o)

libconsteval.a: $(OBJS)
	$(RM) $@
	$(AR) rc $@ $^
	$(RANLIB) $@

clean:	
	rm -rf *.o *~ *.a .#*
