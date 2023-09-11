
all:	ttflag-bad ttflag-ok

clean:	
	rm -f ttflag-bad ttflag-ok ttflag-bad.out ttflag-ok.out

ttflag-bad:  my-main.c tTflag-bad.c
	$(CC) -g -o ttflag-bad my-main.c mymain.c tTflag-bad.c -I .

ttflag-ok:  my-main.c tTflag-ok.c
	$(CC) -g -o ttflag-ok my-main.c mymain.c tTflag-ok.c -I .



