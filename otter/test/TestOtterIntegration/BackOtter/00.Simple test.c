/*
 * ./otter.pl --dobackotter 00.Simple test.c
 *
 * Expected output:
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (y < 10): true
 *          Decision: IF (y > 0): true
 *          Decision: nothing: void (void)
 *          Decision: g: void (int y )
 *          Decision: IF (z > -5): true
 *          Decision: f: void (int z )
 *
 * -----------------------------------------
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (y < 10): true
 *          Decision: IF (y > 0): true
 *          Decision: nothing: void (void)
 *          Decision: g: void (int y )
 *          Decision: IF (z > -5): false
 *          Decision: f: void (int z )
 *
 * -----------------------------------------
 */

#pragma expect_abandoned(failure_reached, x > -5)
#pragma expect_abandoned(failure_reached, x <= -5)
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned
#pragma no_other_return
#pragma no_other_exit
/* there may be truncated jobs */

int x;

void __FAILURE(void) { }

void nothing(void) {
    // This function is created to make sure that the cfg-pruning
    // works properly with function calls
}

void g(int y) {
    nothing();
    if (y > 0) {
        if (y < 10) {
            __FAILURE();
        }
    }
}

void f(int z) {
    if (z > -5) {
        g(z);
    } else {
        g(-z);
    }
}

void main(void) {
    __SYMBOLIC(&x);
    f(x);
}
