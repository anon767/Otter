/* This tests that unsigned division of two symbolic variables work correctly. */

#pragma expect_abandoned(division_by_zero)
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)
#pragma no_other_results

int main(void) {
    unsigned x, y, z;

    __SYMBOLIC(&x);
    __SYMBOLIC(&y);

    z = x / y;

    __ASSERT(y != 0);

    if (x > 0) {
        __ASSERT(z >= 0);
        return 1;
    } else { /* x == 0 */
        __ASSERT(z == 0);
        return 2;
    }
}
