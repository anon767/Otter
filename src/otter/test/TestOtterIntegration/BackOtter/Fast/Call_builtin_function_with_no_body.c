/*
 * This test ensures that during BackOtter's callgraph processing, built-in functions, which have no body, are handled
 * appropriately.
 *
 * As of r10258, this test causes BackOtter to crashes.
 */

void __SYMBOLIC(void *);
void __FAILURE(void) __attribute__((used)) { }

int main(void) {
    int x;
    __SYMBOLIC(&x);
    return 0;
}
