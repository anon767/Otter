/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=2 -Ilibc test/TestBackOtter/test2.c
 * (It doesn't work with --max-abandoned=1, since in that case an infeasible failing path hides the feasible one.)
 *
 * Expected output:
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (*x): true
 *          Decision: f: void (int *x )
 *
 * -----------------------------------------
 */
#include "otter.h"

void nothing() {}

void f(int* x) {
    __EVAL(x);
    if (*x) {
        __FAILURE();
    }
}

void main() {
    int x;
    __SYMBOLIC(&x);
    f(&x);
}
