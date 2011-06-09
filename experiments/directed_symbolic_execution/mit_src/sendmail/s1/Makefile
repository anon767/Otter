
all:	ca-bad ca-ok ca-otter

clean:	
	rm -rf ca-bad ca-ok ca-otter ca-bad.out ca-ok.out ca-otter.out

ca-otter: crackaddr-otter.c
	$(CC) -g -o ca-otter crackaddr-otter.c

ca-bad: crackaddr-bad.c
	$(CC) -g -o ca-bad crackaddr-bad.c

ca-ok: crackaddr-ok.c
	$(CC) -g -o ca-ok crackaddr-ok.c

