/* Test that memcpy checks that its source and destination don't overlap */
#pragma expect_abandoned(failure("memcpy: src and dest overlap"))
#pragma no_other_results

int main() {
  int a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  memcpy(&a[4], &a[1], 5*sizeof(int)); // Fail because src and dest overlap
  return 0;
}
