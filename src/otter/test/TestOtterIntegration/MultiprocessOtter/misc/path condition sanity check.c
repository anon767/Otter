#pragma expect_return()
#pragma no_other_abandoned

int main(void) {
    int x;
    __SYMBOLIC(&x);
    __ASSUME(x);
    __ASSERT(x);
    return 0;
}
