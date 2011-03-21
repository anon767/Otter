/* Test Otter's handling of division by zero. There should one error path (x==0)
	 and only one other path, because 0/x is 0 if x is nonzero. */
#pragma expect_abandoned(division_by_zero)
#pragma expect_return(__return_code__ == 1)
#pragma no_other_results

int main() {
	int x;
	__SYMBOLIC(&x);
	if (0/x) return 0;
	return 1;
}
