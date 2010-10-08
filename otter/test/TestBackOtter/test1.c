/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=2 test/TestBackOtter/test1.c
 * (TODO: see why it doesn't work with --max-abandoned=1)
 */
#include "otter.h"

void nothing() {
    // This function is created to make sure that the cfg-pruning
    // works properly with function calls
}

void g(int x) {
    nothing();
    if (x > 0) {
        if (x < 10) {
            __ASSERT(0);
        }
    }
}

void f(int x) {
    if (x > -5) {
        g(x);
    } else {
        g(-x);
    }
}

void main() {
    int x;
    __SYMBOLIC(&x);
    f(x);
}
