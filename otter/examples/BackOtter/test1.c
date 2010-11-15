/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=(any) -Ilibc test/TestBackOtter/test1.c
 *
 * Expected output:
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (x < 10): true
 *          Decision: IF (x > 0): true
 *          Decision: nothing: void (void)
 *          Decision: g: void (int x )
 *          Decision: IF (x > -5): true
 *          Decision: f: void (int x )
 *
 * -----------------------------------------
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (x < 10): true
 *          Decision: IF (x > 0): true
 *          Decision: nothing: void (void)
 *          Decision: g: void (int x )
 *          Decision: IF (x > -5): false
 *          Decision: f: void (int x )
 *
 * -----------------------------------------
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
            __FAILURE();
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
