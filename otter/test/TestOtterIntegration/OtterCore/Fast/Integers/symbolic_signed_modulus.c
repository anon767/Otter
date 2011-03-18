/* This tests that signed modulus of two symbolic variables work correctly. */

#pragma expect_abandoned(division_by_zero)
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)
#pragma expect_return(__return_code__ == 3)
#pragma no_other_results

int main(void) {
    int x, y, z;
    
    __SYMBOLIC(&x);
    __SYMBOLIC(&y);
    
    z = x % y;
        
    __ASSERT(y != 0);

    /* TODO: test z against y; that seems to hit a bug in STP (as of SVN r1213) */
    if (x > 0) {
        __ASSERT(z >= 0);
        return 1;
    } else if (x < 0) {
        __ASSERT(z <= 0);
        return 2;
    } else {
        __ASSERT(z == 0);
        return 3;
    }
}   
