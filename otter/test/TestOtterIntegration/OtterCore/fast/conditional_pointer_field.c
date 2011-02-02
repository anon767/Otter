/* This tests that conditional pointers in fields are correctly pruned to remove invalid leaves before dereference. */
#pragma expect_abandoned(failure("Dereference something not an address (bytearray)"))
#pragma expect_return()
#pragma no_other_results

void nop(void * x) {}
int main(void) {
    int b, x;
    struct { char a; int *f; } y;
    __SYMBOLIC(&b);
    y.f = b ? 0 : &x;

     /* this leads to an abandoned for the conditional null */
    nop(*y.f);

    /* this should not lead to another abandoned if the conditional null is correctly pruned away */
    nop(*y.f);
    return 0;
}
