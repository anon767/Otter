/* Adapted from cil/test/small1/enum_ptr.c */

typedef enum { A, B, C } T;
void foo(T * x) {
}
int main(void) {
    T y;
    foo(&y);
    return 0;
}