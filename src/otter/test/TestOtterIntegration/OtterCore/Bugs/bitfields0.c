/* Test Otter's handling of overflow with bit-fields. I suspect this
   is undefined behavior, although it's a bit hard to tell from the
   spec. But, gcc does what this test case indicates, and Otter
   handles overflow like gcc in normal types, it should probably do so
   for bit-fields, too. */
#pragma expect_return()
#pragma no_other_results

int main() {
  struct s { int a:1; } s;
  s.a = 0;
  s.a++;
  __ASSERT(s.a == -1);
  return 0;
}
