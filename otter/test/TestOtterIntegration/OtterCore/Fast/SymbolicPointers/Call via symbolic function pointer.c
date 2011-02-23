#pragma entry_function("foo")
#pragma time_limit(1)
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)
#pragma expect_abandoned(failure("Invalid function pointer")) /* for the null case */
#pragma no_other_abandoned

/* This tests that function addresses can be assigned to pointers and called. */

int (*f)(void);
int baz(void) {
    return 1;
}
int bar(void) {
    return 2;
}
int foo(void) {
    return f();
}
int main(void) {
    f = &bar;
    f = &baz;
    return foo();
}
