#ifndef _OTTER_BUILTINS_H
#define _OTTER_BUILTINS_H

#include <stdint.h>
#include <stddef.h>

/* Cause Otter to exit with a failure. */
void __FAILURE() {}

/* __ASSERT(exp) is similar to 'if (!exp) abort();'.
    That is, __ASSERT(exp) checks if the argument is:
    true/nonzero -- do nothing
    false/zero -- exit with a failure
    unknown (possibly true, possibly false, depending on symbolic values) --
    report an error but continue executing, assuming that the checks passed. */
void __ASSERT(_Bool);

/* __ASSUME(exp) is similar to 'if (!exp) exit();'.
    That is, __ASSUME(exp) checks if the argument is:
    true/nonzero -- do nothing
    false/zero -- silently terminate the current execution path
    unknown (possibly true, possibly false, depending on symbolic values) --
    continue executing, assuming that the exp is true. */
void __ASSUME(_Bool);

/* What do we think about this? Are __ASSUME and __ASSERT really
    primitives? Even if not, maybe we want to mark them as special
    'abort's or 'exit's somehow? */
//#define __ASSERT(x) ((x) || abort())
//#define __ASSUME(x) ((x) || exit())

/* __SYMBOLIC is currently overloaded in a somewhat awkward way that
    doesn't quite respect its C type.
    There are 2 ways to use __SYMBOLIC:
    (1) '__SYMBOLIC(&x);' gives x a fresh symbolic value and associates
    that value with the variable x.
    (2) 'x = __SYMBOLIC(n);' assigns to x a fresh symbolic value which
    is not associated to any variable. If n > 0, n is the number of
    symbolic bytes desired; if n <= 0, a number of symbolic bytes equal
    to the size of x is returned. To prevent truncation if x has a wide
    integer type, __SYMBOLIC is declared to return a uintmax_t. */
uintmax_t __SYMBOLIC(void *);

// I think we all agree that we should eventually use this instead.
/* __SYMBOLIC(p, n), where n is a size_t, sets n bytes starting at
    address p to fresh symbolic values. __SYMBOLIC(p) is shorthand for
    __SYMBOLIC(p, sizeof(*p)). */
//void __SYMBOLIC(void *, ...);

/* __EVALSTR(str, len) causes Otter interpret str as a string and
	 print out its value, but to stop at len bytes even if no null byte
	 was encountered. */
void __EVALSTR(char *, size_t);

#endif
