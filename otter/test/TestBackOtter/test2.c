/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=(any>1) -Ilibc test/TestBackOtter/test2.c
 * (It doesn't work with --max-abandoned=1, since in that case an infeasible failing path hides the feasible one.)
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
