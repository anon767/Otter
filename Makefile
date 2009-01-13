
SUBDIRS=cil ocamlstp stp camlidl

all : cil

cil : cil/bin/cilly.exe
cil/bin/cilly.exe : cil/Makefile
	make -C cil
cil/Makefile : CONFIGURE_FLAGS=EXTRALIBDIRS='../camlidl/runtime ../stp/lib ../ocamlstp'
cil/Makefile : ocamlstp


ocamlstp : ocamlstp/stpvc.a
ocamlstp/stpvc.a : stp camlidl
	make -C ocamlstp CAMLIDL='../camlidl/compiler/camlidl' \
	                 LIBDIRS='../camlidl/runtime ../stp/lib' \
					 INCDIRS='../camlidl/runtime ../stp/c_interface'


stp : stp/lib/libstp.a
stp/lib/libstp.a : stp/Makefile
	make -C stp
stp/Makefile : CONFIGURE_FLAGS=--with-prefix=.


camlidl : camlidl/compiler/camlidl
camlidl/compiler/camlidl :
	make -C camlidl


clean :
	$(foreach foo,$(SUBDIRS),make -C $(foo) clean;)


%/Makefile : %/Makefile.in Makefile
	cd $(@D) && ./configure $(CONFIGURE_FLAGS)

%/Makefile : Makefile
	cd $(@D) && ./configure $(CONFIGURE_FLAGS)

