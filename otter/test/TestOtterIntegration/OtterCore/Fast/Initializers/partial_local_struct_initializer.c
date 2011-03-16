/* This tests that variables with partial initialization lists have the remaining elements initialized as zero. (C99 6.7.8.21) */

#expect_no_other_abandoned

int main(void) {
    struct { int a; int b; } x = { 1 };
    __ASSERT(x.b == 0);
    return 0;
}
