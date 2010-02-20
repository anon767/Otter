SYMEXE=env OCAMLRUNPARAM=b CILLY_DONT_COMPILE_AFTER_MERGE= \
      ../bin/cilly \
	  -I../libc \
	  -fno-builtin \
	  --domakeCFG \
	  --merge \
	  --keepmerged \
	  --noPrintLn \
	  --doexecute \
	  --printAssign \
	  ../libc/ALL.c
#	  ../libc/sys/NETWORK.c

CFLAGS=-g -Wall

all: httpd

httpd: httpd.c
	gcc -W -Wall -o httpd httpd.c

test-httpd: httpd.c

symtest-httpd: httpd.c

clean:
	rm -f httpd test-httpd.bintest

test-%: test-%.bintest
	@-echo Running $@...;\
	./$<

test-%.bintest: CPPFLAGS+=-include symtest/symtest_mock.h
test-%.bintest: %.c symtest/symtest.c symtest/symtest.h symtest/symtest_mock.c symtest/symtest_mock.h
	$(LINK.c) $^ $(LOADLIBES) $(LDLIBS) -o $@

symtest-%: CC=$(SYMEXE)
symtest-%: CPPFLAGS+=-include symtest/symtest_mock.h
symtest-%: %.c symtest/symtest.c symtest/symtest.h symtest/symtest_mock.c symtest/symtest_mock.h
	$(LINK.c) $^ $(LOADLIBES) $(LDLIBS) -o $@

