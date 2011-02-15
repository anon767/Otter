/* Check that memcpy gets intercepted and implemented internally in Otter */

#pragma expect_return()
#pragma no_other_results

int main() {
  unsigned int a[1] = {1}, b[2] = {2, 3};
  memcpy(a, b, sizeof(a));
  __ASSERT(a[0] == 2);
  return 0;
}
