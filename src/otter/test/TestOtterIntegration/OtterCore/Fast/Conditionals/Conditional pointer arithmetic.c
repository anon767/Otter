/* This tests that pointer arithmetic can be performed on conditional pointers containing null. */
#pragma expect_return()
#pragma no_other_results

int main(void) {
    int b, x[2];
    struct { char a; int *f; } y;
    __SYMBOLIC(&b);
    y.f = b ? 0 : &x[0];

    /* these should work without errors */
    y.f++;
    y.f--;
    return 0;
}
