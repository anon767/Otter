#pragma expect_abandoned(out_of_bounds)
#pragma no_other_results

char x[2];
int i;
int main() {
	i = 5;
  return x[i];
}
