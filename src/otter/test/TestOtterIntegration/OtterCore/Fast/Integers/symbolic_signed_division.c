/* This tests that signed division of two symbolic variables work correctly. */

#pragma expect_abandoned(division_by_zero)
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)
#pragma expect_return(__return_code__ == 3)
#pragma expect_return(__return_code__ == 4)
#pragma expect_return(__return_code__ == 5)
#pragma expect_return(__return_code__ == 6)
#pragma no_other_results

int main(void) {
    int x, y, z;

    __SYMBOLIC(&x);
    __SYMBOLIC(&y);

    z = x / y;

    __ASSERT(y != 0);

    if (x > 0 && y > 0) {
        __ASSERT(z >= 0);
        return 1;
    } else if (x > 0 && y < 0) {
        __ASSERT(z <= 0);
        return 2;
    } else if (x < 0 && y > 0) {
        __ASSERT(z <= 0);
        return 3;
    } else if (x == 0x80000000 && y == -1) {
        /* turns out that this actually overflows, should this be an error in Otter? */
        __ASSERT(z == 0x80000000);
        return 4;
    } else if (x < 0 && y < 0) {
        __ASSERT(z >= 0);
        return 5;
    } else { /* x == 0 */
        __ASSERT(z == 0);
        return 6;
    }
}
