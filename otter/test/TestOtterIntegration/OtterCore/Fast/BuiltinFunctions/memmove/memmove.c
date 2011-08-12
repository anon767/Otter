/* Test that memmove works even when source and destination overlap */
#pragma expect_return()
#pragma no_other_results

typedef unsigned long size_t;
void * memmove(void * dest, void * src, size_t len);

int main() {
  int a[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  memmove(&a[4], &a[1], 5*sizeof(int)); // Overwrite 4,5,6,7,8 with 1,2,3,4,5
  __ASSERT(a[0] == 0);
  __ASSERT(a[1] == 1);
  __ASSERT(a[2] == 2);
  __ASSERT(a[3] == 3);
  __ASSERT(a[4] == 1);
  __ASSERT(a[5] == 2);
  __ASSERT(a[6] == 3);
  __ASSERT(a[7] == 4);
  __ASSERT(a[8] == 5);
  __ASSERT(a[9] == 9);
  return 0;
}
