/*
 * ./otter.pl --dobackotter 01.Simple test.c
 *
 * Expected output:
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (*x): true
 *          Decision: f: void (int *x )
 *
 * -----------------------------------------
 */

#pragma expect_abandoned(target_reached, x != 0)
#pragma expect_return()
#pragma no_other_abandoned
#pragma no_other_return
#pragma no_other_exit
/* there may be truncated jobs */

int x;

void __FAILURE(void) { }

void f(int* x) {
    if (*x) {
        __FAILURE();
    }
}

int main(void) {
    __SYMBOLIC(&x);
    f(&x);
    return 0;
}
