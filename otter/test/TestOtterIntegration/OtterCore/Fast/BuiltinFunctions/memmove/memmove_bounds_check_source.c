/* Check that memmove does a bounds check on the src. */

#pragma expect_abandoned(out_of_bounds)
#pragma no_other_results

typedef unsigned long size_t;
void * memmove(void * dest, void * src, size_t len);

int main() {
  unsigned int a[1] = {1}, b[2] = {2, 3};
  memmove(b, a, sizeof(b)); // Read past end of a
  return 0;
}
