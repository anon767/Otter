# This forces redis to use malloc, free, etc., instead of zmalloc, zfree, etc.
zmalloc=-DNO_ZMALLOC -Dzmalloc=malloc -Dzrealloc=realloc -D'zcalloc(size)=calloc(1,size)' -Dzfree=free -Dzstrdup=strdup

OTTER_TRUNK=$(CURDIR)/../..

files := redis-grammar.c redis-no-varargs-grammar.c list-tests-client.c src/redis-server src/redis-server_comb.c redis-functions

all: $(files)

list-tests-client.c: src/redis-server generate-test-client
	./generate-test-client unit/type/list > list-tests-client.c

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
	deps/linenoise/linenoise_example_comb.c $(files)
