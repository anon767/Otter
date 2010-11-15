/*
 * An example showing when BackOtter does more work
 * than regular forward execution.  This is when the call
 * path really depends carefully on values passed to
 * intermediate calls.
 */

#include "otter.h"

void level4(int x) {
    if (x == 9) __FAILURE();
}
void level3(int x) {
    int n; __SYMBOLIC(&n);
    level4(n?x+1:x-1);
}
void level2(int x) {
    int n; __SYMBOLIC(&n);
    level3(n?x+1:x-1);
}
void level1(int x) {
    int n; __SYMBOLIC(&n);
    level2(n?x+1:x-1);
}
void level0(int x) {
    int n; __SYMBOLIC(&n);
    level1(n?x+1:x-1);
}
void main() {
    level0(5);
}
