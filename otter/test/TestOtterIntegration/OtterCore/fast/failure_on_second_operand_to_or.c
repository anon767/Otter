/* This tests that short-circuiting an || because of a failed second
	 operand returns 1, and not the first operand. */

#pragma expect_abandoned(failure(""))
#pragma expect_return()
#pragma no_other_results

int main() {
	int x, *p = 0;
	__SYMBOLIC(&x);
	__ASSERT((x + 1 || *p) == 1);
	return 0;
}
