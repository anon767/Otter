/* Test Otter's handling of symbolically sized mallocs. */
#pragma expect_abandoned(out_of_bounds)
#pragma expect_abandoned(failure("Conditional depends on undefined value"))
#pragma no_other_results

int main() {
    unsigned int x;
    __SYMBOLIC(&x);
    int *p = malloc(x * sizeof(*p));
    int i;
    __SYMBOLIC(&i);
    if (p[i]) __ASSERT(0); // This assertion should not be reachable: Otter should flag an error when testing p[i].
    return 0;
}
