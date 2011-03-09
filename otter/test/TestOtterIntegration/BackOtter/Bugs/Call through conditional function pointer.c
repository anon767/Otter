/* This causes an assertion failure as of r11614. */

#pragma expect_abandoned(target_reached, x == 1)
#pragma expect_abandoned(target_reached, x == 2)
#pragma no_other_results

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
