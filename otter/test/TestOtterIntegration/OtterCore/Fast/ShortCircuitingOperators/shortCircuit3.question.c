/* This test should have one successful return, corresponding to !x,
	 and one failing path, corresponding to x (because of the
	 dereference of the null pointer p) */
#pragma expect_return(!x)
#pragma expect_abandoned(failure("Dereference"), x)
#pragma no_other_results

int x, *p=0;
int main() {
	__SYMBOLIC(&x);
	return x ? *p : 0;
}
