/* This tests that unsigned modulus of two symbolic variables work correctly. */

#pragma expect_abandoned(division_by_zero)
#pragma expect_return(__return_code__ == 1)
#pragma no_other_results

int main(void) {
    unsigned x, y, z;
    
    __SYMBOLIC(&x);
    __SYMBOLIC(&y);
    
    z = x % y;
        
    __ASSERT(y != 0);
    /* TODO: test z against y; that seems to hit a bug in STP (as of SVN r1213) */
    return 1;
}   
