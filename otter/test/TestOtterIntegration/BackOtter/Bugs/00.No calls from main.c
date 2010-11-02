/*
 * This test ensures that during BackOtter's callgraph processing, functions which do not make any function calls are
 * handled appropriately.
 *
 * As of r10258, this test causes BackOtter to crashes.
 */

int main(void) {
    return 0;
}
