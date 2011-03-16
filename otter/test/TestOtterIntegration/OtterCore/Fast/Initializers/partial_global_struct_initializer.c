/* This tests that variables with partial initialization lists have the remaining elements initialized as zero. (C99 6.7.8.21) */

#expect_no_other_abandoned

struct { int a; int b; } x = { 1 };

int main(void) {
    __ASSERT(x.b == 0);
    return 0;
}
