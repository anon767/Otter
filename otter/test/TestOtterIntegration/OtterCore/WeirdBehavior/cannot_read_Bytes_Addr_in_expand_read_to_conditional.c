/*
 * This tests that BytesUtility.expand_read_to_conditional does not unnecessarily expand reads to more conditionals
 * that contain reads, that would lead to callers recursively calling BytesUtility.expand_read_to_conditional.
 *
 * Alternatively, callers of BytesUtility.expand_read_to_conditional should avoid calling it unnecessarily, or
 * otherwise detect and prune unnecessary branches in the returned conditional.
 *
 * As of r10010, this file leads to an infinite recursion in Operator.opPI which calls BytesUtility.expand_read_to_conditional.
 */
#pragma no_bounds_checking
#pragma expect_abandoned(failure("Dereference something not an address")) /* for z == 0 */
#pragma expect_abandoned(failure("Dereference something not an address")) /* for z[1] == 0 */
#pragma no_other_abandoned

int main(void) {
    int k;
    double x, *y[1], **z;
    __SYMBOLIC(&k);
    y[0] = &x;
    z = k ? 0 : y;

    z[1][0] = 1;
    return 0;
}
