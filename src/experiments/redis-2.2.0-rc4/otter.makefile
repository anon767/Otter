# This forces redis to use malloc, free, etc., instead of zmalloc, zfree, etc.
zmalloc=-DNO_ZMALLOC -Dzmalloc=malloc -Dzrealloc=realloc -D'zcalloc(size)=calloc(1,size)' -Dzfree=free -Dzstrdup=strdup

OTTER_TRUNK=$(CURDIR)/../..

files := redis-grammar.c redis-no-varargs-grammar.c src/redis-server src/redis-server_comb.c redis-functions\
test-functions.c test-functions-symbolic.c test-functions.h

all: $(files) grouped-clients

grouped-clients:
	num_grouped_clients=`grep -c ^void hand-grouped-test-functions.h`; \
	mkdir -p grouped-clients && \
	for i in `jot $$num_grouped_clients`; do\
	  j=$$((i+1)); sed -n $${j}p hand-grouped-test-functions.h | ./make-client > grouped-clients/client-$${i}.c;\
	done

test-functions.h: test-functions.c
	sed -n 's/^\(void .*(void)\).*/\1;/p' test-functions.c > test-functions.h

test-functions.c: OTTER_SYMBOLIC=0
test-functions-symbolic.c: OTTER_SYMBOLIC=1
test-function%.c: src/redis-server generate-test-functions
	export OTTER_SYMBOLIC
	OTTER_SYMBOLIC=$(OTTER_SYMBOLIC) ./generate-test-functions unit/type/list > $@

# Create the redis-server executable, but delete everything else
src/redis-server:
	$(MAKE) || $(MAKE) clean all
	mv src/redis-server .
	$(MAKE) clean
	mv redis-server src

# This doesn't really depend on src/redis-server, but we have to create that
# first so that intermediate files don't get in the way, and so that we end up
# with the .a files that CIL generates, rather than the ones that ar creates.
src/redis-server_comb.c: src/redis-server
	$(MAKE) CC="$(OTTER_TRUNK)/newlib-1.19.0/otter/cilly-with-libc $(zmalloc)" uname_S= AR='$(OTTER_TRUNK)/cil/bin/cilly --merge --mode=AR'

redis-functions: src/redis-server_comb.c
	../../otter/otter.pl --dofindFns src/redis-server_comb.c > redis-functions

redi%-grammar.c:
	../../otter/src/OcamlUtilities/Grammar/grammar < redi$*.grammar > $@

with-zmalloc: zmalloc=
with-zmalloc: all

clean:
	$(MAKE) clean
	rm -rf $(wildcard src/*_comb.c) src/___extra_files deps/hiredis/libhiredis.a\
	deps/linenoise/linenoise_example_comb.c $(files) grouped-clients
