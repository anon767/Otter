/*
 * This test passes if double is changed to int.
 */
#pragma entry_function("f")
#pragma no_bounds_checking
#pragma expect_abandoned(failure("Dereference something not an address")) /* for n == 0 */
#pragma expect_abandoned(failure("Dereference something not an address")) /* for n[1] == 0 */
#pragma no_other_abandoned

double f(double** n) {
  return n[1][0];
}

int main() {
  double** ptr = malloc(80);
  ptr[1] = malloc(80);
  f(ptr);
  return 0;
}
