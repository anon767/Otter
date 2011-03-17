/* This tests that a struct in an array may be assigned to a struct of the same type. */

struct { int a; int b; } x[2], y;

int main(void) {
    int n;
    __SYMBOLIC(&n);
    __ASSUME(n == 0 || n == 1);

    __SYMBOLIC(&x);
    __SYMBOLIC(&y);

    x[n] = y;
    __ASSERT(x[n].a == y.a);
    __ASSERT(x[n].b == y.b);
    return 0;
}