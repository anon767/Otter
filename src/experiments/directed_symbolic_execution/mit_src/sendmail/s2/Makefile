# make CC="$trunk/newlib-1.19.0/otter/cilly-with-libc" LD="$trunk/newlib-1.19.0/otter/cilly-with-libc"
LD = ld --wrap getpwent 

all:	ge-bad ge-ok

clean:	
	rm -f ge-bad ge-ok ge-bad.out ge-ok.out *.o

ge-bad: main-bad.c  recipient-bad.c  util-bad.c mywrapper.c
	$(CC) -g -c main-bad.c  -I .
	$(CC) -g -c recipient-bad.c -I .  
	$(CC) -g -c util-bad.c  -I .
	$(CC) -g -c mywrapper.c  -I .
	$(LD) -r -o ge-bad.o main-bad.o recipient-bad.o util-bad.o mywrapper.o
	$(CC) ge-bad.o -o ge-bad

ge-ok: main-ok.c  recipient-ok.c  util-ok.c mywrapper.c
	$(CC) -g -c main-ok.c  -I .
	$(CC) -g -c recipient-ok.c -I .  
	$(CC) -g -c util-ok.c  -I .
	$(CC) -g -c mywrapper.c  -I .
	$(LD) -r -o ge-ok.o main-ok.o recipient-ok.o util-ok.o mywrapper.o
	$(CC) ge-ok.o -o ge-ok


