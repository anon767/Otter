/* Test Otter's handling of symbolically sized mallocs. */
#pragma expect_return()
#pragma no_other_results

int main() {
    unsigned int x;
    __SYMBOLIC(&x);
    __ASSUME(x == 2);
    int *p = malloc(x * sizeof(*p));
    p[1] = 0;
    return 0;
}
