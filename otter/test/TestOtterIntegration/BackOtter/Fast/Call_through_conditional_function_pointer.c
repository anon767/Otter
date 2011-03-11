/* This tests that BackOtter tracks jobs that fork on function pointers. */

#pragma expect_abandoned(target_reached, x == 1)
#pragma expect_abandoned(target_reached, x == 2)
#pragma no_other_abandoned

int x;

void __FAILURE(void) {}

void bar(void) {
    x = 1;
    __FAILURE();
}

void foo(void) {
    x = 2;
    __FAILURE();
}

int main(void) {
    int b;
    void (*fn)(void);
    __SYMBOLIC(&b);
    fn = b ? foo : bar;
    fn();
    return 0;
}
