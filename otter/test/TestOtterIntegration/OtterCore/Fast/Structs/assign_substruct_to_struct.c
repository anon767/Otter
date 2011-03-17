/* This tests that a field of a struct may be assigned to a struct of the same type. */

struct s { int a; int b; } x;
struct { int a; struct s b; int c; } y;

int main(void) {
    __SYMBOLIC(&x);
    __SYMBOLIC(&y);
    x = y.b;
    __ASSERT(x.a == y.b.a);
    __ASSERT(x.b == y.b.b);
    return 0;
}