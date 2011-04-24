/* Test freeing pointers within symbolic-length mallocs at a symbolic offset */

// Assigning to p[1] will fail a bounds check, as will p[y]
#pragma expect_abandoned(out_of_bounds)
#pragma expect_abandoned(out_of_bounds)

// Freeing p[y] fails if 1 < y < x because p[y] is uninitialized
#pragma expect_abandoned(failure("Freeing something that is not a valid pointer"))

// Assigning through p[0] fails if y == 0
#pragma expect_abandoned(failure("Dereference"))

// Assigning through p[0] succeeds if y == 1
#pragma expect_return()

// This test hits an infinite loop as of r12170
#pragma time_limit(1)

#pragma no_other_results

int main() {
    int x;
    __SYMBOLIC(&x);
    char **p = malloc(x);
    p[1] = malloc(1);
    p[0] = malloc(1);
    int y;
    __SYMBOLIC(&y);
    free(p[y]);
    *p[0] = 0;
    return 0;
}
