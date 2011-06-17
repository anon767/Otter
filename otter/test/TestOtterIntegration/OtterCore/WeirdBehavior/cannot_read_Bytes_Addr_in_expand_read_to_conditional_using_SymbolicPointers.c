/*
 * This tests that BytesUtility.expand_read_to_conditional does not unnecessarily expand reads to more conditionals
 * that contain reads, that would lead to callers recursively calling BytesUtility.expand_read_to_conditional.
 *
 * Alternatively, callers of BytesUtility.expand_read_to_conditional should avoid calling it unnecessarily, or
 * otherwise detect and prune unnecessary branches in the returned conditional.
 *
 * As of r10010, this file leads to an infinite recursion in Operator.opPI which calls BytesUtility.expand_read_to_conditional.
 *
 * As of r10149, this test case is detected and simply terminated with a "Not a valid array" failure in
 * BytesUtility.expand_read_to_conditional.
 */
#pragma entry_function("f")
#pragma no_bounds_checking
#pragma expect_abandoned(failure("Dereference something not an address")) /* for n == 0 */
#pragma expect_abandoned(failure("Dereference something not an address")) /* for n[1] == 0 */
#pragma no_other_abandoned

typedef unsigned long size_t;
void * malloc(size_t size);

double f(double** n) { /* This test passes if double is changed to int. */
  return n[1][0];
}

int main() {
  double** ptr = malloc(80);
  ptr[1] = malloc(80);
  f(ptr);
  return 0;
}
