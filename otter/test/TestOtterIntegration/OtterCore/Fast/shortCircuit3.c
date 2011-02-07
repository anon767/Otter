/* This test should have one successful return, corresponding to !x,
	 and one failing path, corresponding to x (because of the
	 dereference of the null pointer p) */
#pragma expect_return()
#pragma expect_abandoned(failure(""))
#pragma no_other_results

int main() {
	int x, *p=0;
	__SYMBOLIC(&x);
	return x && *p;
}
