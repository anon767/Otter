/* Test freeing a pointer at a symbolic offset within an array of pointers */

// p[x] will fail a bounds check
#pragma expect_abandoned(out_of_bounds)

// Assigning through p[0] fails if x == 0
#pragma expect_abandoned(failure("Dereference"))

// Assigning through p[0] succeeds if x == 1
#pragma expect_return()

#pragma no_other_results

int main() {
    char **p = malloc(2*sizeof(*p));
    p[0] = malloc(1);
    p[1] = malloc(2);
    int x;
    __SYMBOLIC(&x);
    free(p[x]);
    *p[0] = 0;
    return 0;
}
