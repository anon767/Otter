typedef enum { A, B, C } T;
void foo(T * x) {
}
void bar(void) {
    T y;
    foo(&y);
}