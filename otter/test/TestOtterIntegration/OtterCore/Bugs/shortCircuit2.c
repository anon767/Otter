// j < 10
#pragma expect_return(j < 10)
// j >= 10 but i < 2, so a[j] was not evaluated
#pragma expect_return(j >= 10, i < 2)
// j >= 10 and i >= 2, so a[j] was an out-of-bounds error
#pragma expect_abandoned(out_of_bounds, j >= 10, i >= 2)
#pragma no_other_results

char a[10];
int i, j;
int main() {
	__SYMBOLIC(&i);
	__SYMBOLIC(&j);
	if (i < 2 && a[j]) {
		__ASSERT(0); // Unreachable, because a[j] is either out-of-bounds or 0
	}
	if (j < 10) {
		return 0;
	} else {
		// j >= 10, so i must be less than 2; otherwise, a[j] would have been out-of-bounds
		__ASSERT(i < 2);
	}
	return 1;
}
