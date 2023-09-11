/* Test Otter's handling of bitfields. */
#pragma expect_return()
#pragma no_other_results

int main() {
  struct s { int a:5, b:5; } s;
  s.a = 1;
  s.b = 0;
  __ASSERT(s.a);
  return 0;
}
