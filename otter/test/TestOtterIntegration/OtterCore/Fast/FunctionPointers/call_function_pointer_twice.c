/* This tests that an error is raised only once for each invalid value in a symbolic function pointer. */
#pragma expect_abandoned(failure("Invalid function pointer"))
#pragma no_other_abandoned

void foo(void) {
}

int main(void) {
    int i;
    void (*f[2])(void) = { foo, 0 };
    __SYMBOLIC(&i);
    __ASSUME(i >= 0 && i < 2);
    f[i]();
    f[i]();
    return 0;
}
