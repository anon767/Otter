/* Test freeing pointers within symbolic-length mallocs at a symbolic offset */

// Assigning to p[1] will fail a bounds check
#pragma expect_abandoned(out_of_bounds)

// Reading p[y] will fail a bounds check
#pragma expect_abandoned(out_of_bounds)

// Freeing p[y] fails if 1 < y < x because p[y] is uninitialized
#pragma expect_abandoned(failure("Freeing something that is not a valid pointer"), one_lt_y_lt_x)

// Assigning through p[0] fails if y == 0
#pragma expect_abandoned(failure("Dereference a dangling pointer"), y_eq_0)

// Assigning through p[0] succeeds if y == 1
#pragma expect_return(y_eq_1)

// This test hits an infinite loop as of r12170
#pragma time_limit(1)

#pragma no_other_results

/* OtterPragmaTests can't handle sizeof, nor can it do unsigned arithmetic.
However, x and y are used as indices and hence get treated as unsigned and get
multiplied by sizeof(char*), and this is sometimes important for stating the
correct assertions. This is a workaround: compute the assertions in the code
itself, put the result in a global int, and put the int in the pragma. */
int one_lt_y_lt_x, y_eq_0, y_eq_1;

int x, y;
int main() {
    __SYMBOLIC(&x);
    __SYMBOLIC(&y);
    // Compute the assertions for the pragmas
    one_lt_y_lt_x = sizeof(char*) < sizeof(char*) * y && sizeof(char*) * y < sizeof(char*) * x;
    y_eq_0 = sizeof(char*) * y == 0 * sizeof(char*);
    y_eq_1 = sizeof(char*) * y == 1 * sizeof(char*);

    char **p = malloc(sizeof(char*) * x);
    p[1] = malloc(1);
    p[0] = malloc(1);
    free(p[y]);
    *p[0] = 0;
    return 0;
}
