/* This tests that structs of the same type can be assigned correctly to each other. */

struct { int a; int b; } x, y;

int main(void) {
    __SYMBOLIC(&x);
    __SYMBOLIC(&y);
    x = y;
    __ASSERT(x.a == y.a);
    __ASSERT(x.b == y.b);
    return 0;
}