/* This tests that calls to built-in functions are intercepted when made through symbolic function pointers */

#pragma expect_return()
#pragma expect_return()
#pragma no_other_results

void *malloc(unsigned int len) { __ASSERT(0); return 0; } // This should never be called because malloc is a built-in.
void *f(unsigned int x) { return 0; }

int main(void) {
  int x;
  __SYMBOLIC(&x);
  void *(*fp)(unsigned int) = x ? malloc : f;
  void *p = fp(5);
  return 0;
}
