kleeobjs = pro_backotter_3.o  pro_distance_1.o  pro_mix_1.o

all : $(kleeobjs)

%.o : %.c
	llvm-gcc --emit-llvm -c -g -DKLEE $<

