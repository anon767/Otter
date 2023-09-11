/* This tests that short-circuiting an || because of a failed second
	 operand returns 1, and not the first operand. */

#pragma expect_abandoned(failure("Dereference"), !(x + 1))
#pragma expect_return(x + 1)
#pragma no_other_results

int x, *p = 0;
int main() {
	__SYMBOLIC(&x);
	__ASSERT((x + 1 || *p) == 1);
	return 0;
}
