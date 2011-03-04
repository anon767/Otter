/* This tests that pointer arithmetic can be performed on conditional pointers. */
#pragma expect_abandoned(failure("plusPI (p1,p2) not of type (addr,int)"))
#pragma expect_return()
#pragma no_other_results

int main(void) {
    int b, x[2];
    struct { char a; int *f; } y;
    __SYMBOLIC(&b);
    y.f = b ? 0 : &x[0];

     /* this leads to an abandoned for the conditional null */
    y.f++;

    /* this should not lead to another abandoned if the conditional null is correctly pruned away */
    y.f--;
    return 0;
}
