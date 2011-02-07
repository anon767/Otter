#pragma expect_abandoned(out_of_bounds)
#pragma expect_return(0 <= i, i < 2)
#pragma no_other_results

char x[2]; // This has to be char, otherwise the multiplication by sizeof(int) that happens means that i could be 0, 1, or some huge number (>= 2**30, or something).
int i;
int main() {
  __SYMBOLIC(&i);
  return x[i];
}
