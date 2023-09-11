/* This tests extension followed by truncation. */

#pragma no_other_abandoned

int main(void) {
    int x;
    long long y;
    short z;
    __SYMBOLIC(&x);
    __ASSUME(x == 0);
    y = !x;
    z = y;
    __ASSERT(z);
    return 0;
}
