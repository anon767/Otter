/* This tests that dereferencing a field of a shared struct works correctly.
   In r12395, this lead to a "Not a valid array" error rather than a "Dereference" error.
*/
#pragma expect_abandoned("Dereference")
#pragma no_other_results

struct t {
    int x;
    struct { int m; } *y;
};
int main(void)  {
    struct t *a = __otter_multi_gmalloc(sizeof(struct t));
    return a->y->m;
}
