Quick-start guide
=================

* Configure, build and test:

        (In the top directory)
        make otter
        make test-otter

* Build and test only Otter (after configuring):

        (In the top/otter directory)
        make
        make test

* Running Otter (after building):

        (In the top/otter directory)
        ./otter.pl [flags] <some c file>

* Adding a new module to Otter:

    1. Add your module `MyModule.ml` to `src/Otter`
    2. Edit `src/Otter.mlpack` and add a line `Otter/MyModule`

* Adding a new test module to Otter:

    1. Add your test module `TestMyModule.ml` to `test/TestOtter`
    2. Edit `test/TestOtter.mlpack` and add a line `TestOtter/TestMyModule`
    3. Edit `test/runtestotter.ml` and add your test module's testsuite to the
       list of tests to run:

            run_test_tt_main begin TestList [
                ...
                TestOtter.TestMyModule.testsuite;
                ...
            ] end

* Adding a new module pack to Otter:

    1. Add the contents of your new module pack to `src` with the following
       organization:

        * `src`
            * `MyModulePack`
                * `_tags`
                * `Module1.ml`
                * `Module2.ml`
            * `MyModulePack.mlpack`

    2. Add a file `MyModulePack/_tags` with a single line:

            <*.cm*>: for-pack(MyModulePack)

    3. Add a file `MyModulePack.mlpack` listing the modules to include:

            MyModulePack/Module1
            MyModulePack/Module2

    4. To export `MyModulePack` in the `Otter.cm[x]a` library, add it to
       `Otter.mllib` as well as the `libs` target in `Makefile.in`.

    5. To have `ocamldoc` documentation automatically generated for
       `MyModulePack`, add it to the `api` target in `Makefile.in`.

* Adding an integration test case to Otter (does not invoke `otter.pl`):

    1. Add your C file test case to one of the directories under
       `test/TestOtterIntegration`:

        * For single-process test cases, add it under `OtterCore`
        * For multi-process test cases, add it under `MultiprocessOtter`

    2. You may create additional directories under `OtterCore` or
       `MultiprocessOtter` to group your test cases.

* Adding an system test case to Otter (invokes `otter.pl`):

    * Similar to the integration tests, but in `test/TestOtterSystem`

* Building Otter's documentation:

    1. Either run `make doc-otter` in the top directory, or `make doc` in
       `otter`
    2. View the documentation in `doc/api`: ocamldoc documentation for various
       module packs will be in `*.docdir`, and a module dependency diagram
       in the `All.dot` GraphViz file

* Debugging Otter:

    1. Build a debug version of Otter:

            (In the top/otter directory)
            make debug

    2. Run the debugger script:

            (In the top/otter directory)
            scripts/debug [flags] <some c file>

        Hopefully, that just works, but if the script gives you trouble, do the
        following (which is basically what the script does, or tries to do):

    2a. Start the debugger in remote debugging mode:

            (In the top/otter directory)
            ocamldebug -s debugsocket -I _build/src/OtterCore -I _build/src \
                -I ../cil/obj/x86_DARWIN -I ../cil/src _product/runotter.d.byte

            (In the debugger)
            set loadingmode manual
            step

    2b. Start Otter in debug mode:

            (In the top/otter directory)
            env CAML_DEBUG_SOCKET=debugsocket \
                ./otter.byte [flags] <some c file>

        The `x86_DARWIN` directory (which is also hard-coded into the debug script)
        is for Macs. Substitute `x86_LINUX` or the like for other systems.


Building Otter
==============

The easiest way of building Otter is from the top directory (i.e., the directory
above the one this file is in). From the top directory, type:

    make otter

This will configure and build Otter as well as the required libraries such as
CIL and STP. Subsequently, the above command will also automatically rebuild
libraries that are updated (e.g., from the code repository) and, for the most
part, it will also properly rebuild the required libraries as well as Otter.
So, this is the recommended way to build Otter.

After building, you should also run Otter's test suite:

    make test-otter

After the first build, which will configure Otter appropriately, Otter itself
may also be rebuilt locally in the `otter` directory:

    cd otter
    make

Likewise, the test suite may also be run locally:

    cd otter
    make test

For programs that make use of the C standard library, libc must be built
separately, from the `top/newlib-1.19.0/otter` directory:

    make -C newlib-1.19.0/otter


Where are the build products?
-----------------------------

Otter is built using [Ocamlbuild][] that is driven by a Makefile. This build
system is set up to compile the source code in the `_build` directory, and
additionally, the actual Otter native code executable itself will be copied to
`_product/runotter.native`. Instead of running this executable directly however,
it is more convenient to run the wrapper script `otter.pl` described in the next
section.

A debug bytecode version of the Otter executable can also be built with the
command:

    make debug

It will be copied to `_product/runotter.d.byte`.


[Ocamlbuild]: <http://nicolaspouillard.fr/ocamlbuild/ocamlbuild-user-guide.html>
              "Ocamlbuild User Guide"


Running Otter
=============

Otter includes a convenient wrapper script `otter.pl` that provides a `gcc`-like
interface to Otter. The basic usage syntax is:

    ./otter.pl [flags] <input files>

For example:

    ./otter.pl --printLittle examples/test-mystrcmp.c

`otter.pl` will first run the input files through the C preprocessor (like
`gcc`), merge all input files, then call the Otter executable on the result.

By default, `otter.pl` will run the native code executable. Alternatively, the
bytecode executable may be invoked in two ways:

    ./otter.pl --bytecode [other flags] <input files>
    ./otter.byte [flags] <input files>

Additional options to Otter can be listed with:

    ./otter.pl --help

Furthermore, many `gcc`-style flags are also supported (although not listed in
the command line help), e.g.:

    ./otter.pl -DSOME_MACRO -include somefile.h -Isomedir

Programs which make use of libc need the library functions to be merged in with
program itself. To this end, build libc generates a script

    newlib-1.19.0/otter/otter-with-libc

which should be used instead of otter.pl. otter-with-libc is simply a wrapper
around otter.pl which provides extra options to link in libc. It accepts
additional flags and input files just as otter.pl does:

    newlib-1.19.0/otter/otter-with-libc [flags] <input files>

One useful flag when using libc is `--doRunRmtmps`:

    newlib-1.19.0/otter/otter-with-libc --doRunRmtmps --dootter [flags] <input files>

(Specifying `--doRunRmtmps` requires you to explicitly specify which flavor of
Otter you want, which otherwise defaults to `--dootter`.) Rmtmps is a CIL module
which removes from the merged file all functions and global variables which are
not reachable from main. If your program uses only a piece of libc, this can
make the merged program much smaller.

Additionally, sometimes it is useful to merge a program with libc first and
then, later, symbolically execute it. To do this, there is a script called
cilly-with-libc in the same directory as otter-with-libc. This can be useful,
for example, when merging a program that has a makefile. If, say, you are
building a program whose makefile produces an executable called `foo`,

    make CC=newlib-1.19.0/otter/cilly-with-libc

will produce a file `foo_comb.c`, which is the program merged using Otter's libc
and POSIX header files. Later, this can be run using otter-with-libc, which
merges the libc and POSIX implementations in with the already-merged file.


Developing Otter
================

The source code to Otter is organized into two directories:

* the `src` directory contains the source code to Otter;
* the `test` directory contains the source code to Otter's test suite.

Both these directories contain one or more directories of library modules to be
compiled into module packs (i.e., module of modules), as well as a driver module
to be compiled into an executable. For example:

* `src`
    * `Otter`
        * `_tags`
        * `Module1.ml`
        * `Module2.ml`
    * `Otter.mlpack`
    * `runotter.ml`

The `Otter` directory contains the source code to various Otter modules, and
`runotter.ml` is the driver module. In addition to the source files, there are
two meta files---`_tags` and `Otter.mlpack`---that instruct Ocamlbuild to
compile the directory into a module pack (e.g., `Otter.cma`). The `_tags` file
contain compilation directives for each `.ml` file in the directory, while the
`Otter.mlpack` file contains a list of modules to compile into a module pack.
`Otter.mlpack` is also used when generating documentation; if desired, an
`Otter.odocl` file may be used to customize the list of modules for which
documentation should be generated. (Unfortunately, these files are not very
well documented in the [Ocamlbuild][] user guide).

When writing a new module, e.g., `NewModule.ml`, put the file into a module
pack directory, and add a reference to it in the corresponding `.mlpack` file.
If the module provides a new CIL feature, add it to the `runotter.ml` driver:

    (* in src/runotter.ml *)
    Cilly.run [
        ...
        Otter.NewModule.feature;
        ...
    ] ()

Likewise, when writing a new test module in the `test` directory, add the new
module's test suite to the `runtestotter.ml` driver:

    (* in test/runtestotter.ml *)
    run_test_tt_main begin TestList [
        ...
        TestOtter.TestNewModule.testsuite;
        ...
    ] end

Note that when writing a module, references to modules in the same directory
should be unqualified:

    (* in Module1.ml *)
    let foo = Module2.bar;;

Whereas references to modules in other directories must be qualified:

    (* in Module1.ml *)
    let foo = OtherDirectory.Module.bar;;

New module packs may also be created by following the above guidelines, with
an additional caveat: due to a (as yet unresolved) quirk in Ocamlbuild, module
packs should be added to both `runotter.ml` and `runtestotter.ml` the under
`OcamlbuildDependencies`. To export module packs as part of the
`Otter.cma`/`Otter.cmxa` libraries, add them to `Otter.mllib` as well as the
`libs` target in `Makefile.in`. To have `ocamldoc` documentation automatically
generated for module packs, add them to the `api` target in `Makefile.in`.


Adding integration or system tests to Otter
===========================================

Otter also contains testing framework for integration testing in
`test/TestOtterIntegration` which invokes only Otter, as well as system testing
in `test/TestOtterSystem` which additionally invokes the `otter.pl` wrapper
for preprocessing.

The core of this testing framework is implemented in
`test/TestUtil/OtterPragmaTests.ml`. Test cases as simply written as C files
with special `#pragma` directives to specify test expectations. Please refer
to `OtterPragmaTests.ml` for documentation of these directives.

To add a new integration test case, simply add a C file to one of the
directories under `test/TestOtterIntegration`:

* for test cases that are single-process, add the file to `OtterCore`;
* for test cases that require multi-process capability, such as calling
      `fork()`, add the file to `MultiprocessOtter`.

You may also create additional directories under `OtterCore` or
`MultiprocessOtter` to group your test cases; the directories may even be
nested in other directories for convenience. The framework will recursively
traverse the entire directory structure to find test cases, and will label the
test cases by their path.

System tests are similarly organized in `test/TestOtterSystem`.


Generating documentation for Otter
==================================

Other than this file, Otter is mainly documented using `ocamldoc` annotations
in the source code. It is more convenient to peruse these documentation as
HTML documents, which may be generated from the top directory by running:

    make doc-otter

or locally by running:

    cd otter
    make doc

This will generate HTML documentation in `otter/doc/api/*.docdir` for
each module pack listed under the `api` target of `Makefile.in` in Otter.
Additionally, this will also generate a module dependency diagram between
Otter's modules as a GraphViz file `otter/doc/All.dot`.


Debugging Otter using the Ocaml debugger
========================================

It takes a few steps to use the Ocaml debugger to debug Otter when using the
`otter.pl` wrapper script. First, build a debug version of Otter:

    make debug

Then, start the Ocaml debugger, giving it the bytecode binary of Otter:

    ocamldebug _product/runotter.d.byte

Before debugging, it's useful to provide the debugger with a list of directories
to find source code files (e.g., `src/Otter`), by typing the following command
to the debugger:

    directory _build/src/Otter [... list of other directories ...]

To allow Otter to be started separately using the `otter.pl` script, set up the
debugger in remote debugging mode with the commands:

    set socket <debug socket>
    set loadingmode manual
    step

where `debug socket` can be any file name which will be used to connect to
Otter. The last line `step` will cause the debugger to wait for Otter to be
launched, and immediately break at the first instruction.

Finally, in a different terminal, start Otter in debug mode, giving it the same
`debug socket` to connect to the debugger:

    env CAML_DEBUG_SOCKET=<debug socket> \
        ./otter.byte [other flags] <input files>
