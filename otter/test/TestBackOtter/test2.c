/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=2 test/TestBackOtter/test2.c
 * (TODO: see why it doesn't work with --max-abandoned=1)
 */
#include "otter.h"

void nothing() {}

void f(int* x) {
    __EVAL(x);
    if (*x) {
        __ASSERT(0);
    }
}

void main() {
    int x;
    __SYMBOLIC(&x);
    f(&x);
}
