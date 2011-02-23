#pragma entry_function("foo")
#pragma time_limit(1)
#pragma expect_return()
#pragma no_other_abandoned

/* This tests that function addresses can be assigned to pointers (void * here, but applicable to function pointers as
   well). Unlike other addresses, function addresses do not refer to storage in memory, but rather like constants. */

void *x;
void bar(void) {
}
void foo(void) {
    x = 1;
}
int main(void) {
    x = &bar;
    foo();
    return 0;
}
