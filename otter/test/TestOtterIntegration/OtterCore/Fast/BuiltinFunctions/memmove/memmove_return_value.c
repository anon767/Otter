/* Check that memmove returns its first argument */

#pragma expect_return()
#pragma no_other_results

typedef unsigned long size_t;
void * memmove(void * dest, void * src, size_t len);

int main() {
  unsigned int a[1] = {1}, b[2] = {2, 3};
  int *p = memmove(&b[1], a, sizeof(a));
  __ASSERT(b[0] == 2);
  __ASSERT(b[1] == 1);
  __ASSERT(p == &b[1]);
  return 0;
}
