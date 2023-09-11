#ifndef _OTTER_H
#define _OTTER_H

#include <stdint.h>

/* Until we upgrade to CIL 1.3.7, which have the type _Bool, we
    represent boolean we represent boolean values using uintmax_t to
    prevent truncation of wide values (such as long long). (We could
    also use intmax_t, but this way widening casts will zero-extend
    rather than sign-extend, which is presumably simpler.) */
typedef _Bool uintmax_t;

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

/* '__SYMBOLIC(&x);' gives x a fresh symbolic value and associates
    that value with the variable x. */
void __SYMBOLIC(void *);

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
