/* Test that long long values can be used as pointer offsets */

#pragma expect_abandoned(out_of_bounds)
#pragma expect_return()
#pragma no_other_results

int main() {
    char a[] = {1,2}, *p = a;
    long long ll;
    __SYMBOLIC(&ll);
    p[ll] = 5;
    return 0;
}
