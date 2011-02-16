/* Check that memcpy returns its first argument */

#pragma expect_return()
#pragma no_other_results

int main() {
  unsigned int a[1] = {1}, b[2] = {2, 3};
  int *p = memcpy(&b[1], a, sizeof(a));
  __ASSERT(b[0] == 2);
  __ASSERT(b[1] == 1);
  __ASSERT(p == &b[1]);
  return 0;
}
