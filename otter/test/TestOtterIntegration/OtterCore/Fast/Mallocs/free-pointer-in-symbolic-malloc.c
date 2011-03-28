/* Test freeing pointers within symbolic-length mallocs at a concrete offset */

// Assigning to p[1] will fail a bounds check
#pragma expect_abandoned(out_of_bounds)

// Assigning through p[1] should fail, because it was freed
#pragma expect_abandoned(failure("Dereference of invalid conditional pointer"))

// Assigning through p[0] should be fine
#pragma expect_return()

#pragma no_other_results

int main() {
    int x;
    __SYMBOLIC(&x);
    char **p = malloc(x);
    p[1] = malloc(1);
    p[0] = malloc(1);
    free(p[1]);
    int y;
    __SYMBOLIC(&y);
    if (y) {
        *p[1] = 0;
    } else {
        *p[0] = 0;
    }
    return 0;
}
