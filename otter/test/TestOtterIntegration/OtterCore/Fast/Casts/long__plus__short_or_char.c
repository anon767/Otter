/* This tests that the result of || is approriately treated as an int like any other (C99 6.3.1.8 and 6.5.14).
 * This is actually handled automatically by CIL which performs integer promotion by inserting explicit casts. */

#pragma no_other_abandoned

int main(void) {
    long x;
    short y;
    char z;
    __SYMBOLIC(&x);
    __SYMBOLIC(&y);
    __SYMBOLIC(&z);
    __ASSUME(x == 1);
    __ASSERT(x + (y || z) > 0);
    return 0;
}
