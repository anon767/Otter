/* Test that negative char values can be used as pointer offsets */

#pragma expect_return()
#pragma no_other_results

int main() {
    int a[] = {1,2}, *p = a+1;
    char c = -1;
    p[c] = 5;
    __ASSERT(a[0] == 5);
    return 0;
}
