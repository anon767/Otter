/* Check that memcpy gets intercepted and implemented internally in Otter */

#pragma expect_return()
#pragma no_other_results

typedef unsigned long size_t;
void * memcpy(void * dest, void * src, size_t len);

int main() {
  unsigned int a[1] = {1}, b[2] = {2, 3};
  memcpy(a, b, sizeof(a));
  __ASSERT(a[0] == 2);
  return 0;
}
