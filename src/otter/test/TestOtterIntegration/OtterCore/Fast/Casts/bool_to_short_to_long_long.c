/* This tests truncation followed by extension. */

#pragma no_other_abandoned

int main(void) {
    int x;
    short y;
    long long z;
    __SYMBOLIC(&x);
    __ASSUME(x == 0);
    y = !x;
    z = y;
    __ASSERT(z);
    return 0;
}
