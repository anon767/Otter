#pragma no_other_abandoned

int main() {
    int x, *p;
    __SYMBOLIC(&x);
    p = x ? &x : 0;
    __ASSERT(1 == ((p+1) - p));
    return 0;
}
