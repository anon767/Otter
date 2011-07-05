/* This tests that short-circuiting works when the first operand is symbolic, and the second operand contains a
 * potentially-null pointer dereference, and that the null-pointer dereference error occurs only once along a path.
 *
 * As of r13052, the second dereference of y below (i.e., when b2 == true && b3 == true) fails again with a
 * null-pointer dereference, despite that the error had already occured at the first dereference, since
 * Expression.evaluate_under_condition did not consider that symbolic pointer dereferences may fail and cause the path
 * condition to be augmented with the non-failing condition, rolling back to an earlier path condition prior to that
 * condition.
 */

#pragma expect_abandoned(failure("Dereference"))
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)
#pragma expect_return(__return_code__ == 3)
#pragma no_other_results

void __SYMBOLIC(void *);

int main(void) {
    _Bool b1, b2, b3;
    int x = 1, *y;
    __SYMBOLIC(&b1);
    __SYMBOLIC(&b2);
    __SYMBOLIC(&b3);
    __SYMBOLIC(&x);
    y = b1 ? &x : 0;
    if(b2 && *y) { /* null-pointer dereference if b2 == true && y == NULL */
        if(b3 && *y) { /* no null-pointer dereference */
            return 1;
        } else {
            return 2;
        }
    } else {
        return 3;
    }
}
