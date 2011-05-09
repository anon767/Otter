
all:	prescan-bad prescan-ok

clean:	
	rm -f prescan-bad prescan-ok prescan-bad.out prescan-ok.out

prescan-bad: prescan-overflow-bad.c
	$(CC) -g -o prescan-bad prescan-overflow-bad.c

prescan-ok: prescan-overflow-ok.c
	$(CC) -g -o prescan-ok prescan-overflow-ok.c


