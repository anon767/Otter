/* This tests that variables with partial initialization lists have the remaining elements initialized as zero. (C99 6.7.8.21) */

#expect_no_other_abandoned

int x[2] = { 1 };

int main(void) {
    __ASSERT(x[1] == 0);
    return 0;
}
