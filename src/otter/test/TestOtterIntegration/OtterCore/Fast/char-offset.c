/* Test that char values can be used as pointer offsets */

#pragma expect_abandoned(out_of_bounds)
#pragma expect_return()
#pragma no_other_results

int main() {
    char a[] = {1,2}, *p = a;
    char c;
    __SYMBOLIC(&c);
    p[c] = 5;
    return 0;
}
