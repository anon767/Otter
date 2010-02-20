AC_PREREQ(2.61)
AC_INIT(cilqual, alpha-0, khooyp@cs.umd.edu)


# Checks for programs.

AC_CHECK_PROG(OCAMLC,ocamlc,ocamlc,no)
if test "$OCAMLC" = no ; then
    AC_MSG_ERROR([Building AC_PACKAGE_NAME requires ocaml version 3.10 or greater.
                  It is available at <http://caml.inria.fr/>).])
fi
OCAMLVERSION=`$OCAMLC -version`
if test "$OCAMLC" \< "3.10" ; then
    AC_MSG_ERROR([Building AC_PACKAGE_NAME requires ocaml version 3.10 or greater.
                  It is available at <http://caml.inria.fr/>).])
fi

AC_CHECK_PROG(OCAMLBUILD,ocamlbuild,ocamlbuild,no)
if test "$OCAMLBUILD" = no ; then
    AC_MSG_ERROR([Building AC_PACKAGE_NAME requires ocamlbuild.
                  It is distributed with Ocaml as of version 3.10 (available at <http://caml.inria.fr/>).])
fi

AC_CHECK_PROG(OCAMLFIND,ocamlfind,ocamlfind,no)
if test "$OCAMLFIND" = no ; then
    AC_MSG_ERROR([Building AC_PACKAGE_NAME requires ocamlfind.
                  It is available at <http://projects.camlcity.org/projects/findlib.html>.])
fi

AC_MSG_CHECKING(checking for ounit)
if ! "$OCAMLFIND" query ounit ; then
    AC_MSG_ERROR([Building AC_PACKAGE_NAME requires ounit to be installed with ocamlfind.
                  It is available at <http://www.xs4all.nl/~mmzeeman/ocaml/>.])
fi


# Add options.

AC_ARG_WITH(cil,
    [AS_HELP_STRING([--with-cil=DIR], [the directory containing the Cil source code])],
    [CIL_SRCDIR="$withval"],
    AC_MSG_ERROR(Please provide the directory containing the Cil source code using --with-cil=DIR.)
)

AC_SUBST(OCAMLBUILD)
AC_SUBST(OCAMLFIND)
AC_SUBST(CIL_SRCDIR)
AC_SUBST(EXTRALIBDIRS)
AC_CONFIG_FILES(Makefile)
AC_OUTPUT
