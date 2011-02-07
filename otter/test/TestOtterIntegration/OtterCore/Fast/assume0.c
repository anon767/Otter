#pragma expect_return()
#pragma no_other_results
// The __ASSUME should eliminate the other path.

int main() {
	int x;
	__SYMBOLIC(&x);
	int *y = x == -1 ? 0 : &x;
	if (x < 0) {
		__ASSUME(*y, x == 0);
    /* The possible error of dereferencing y when it's null doesn't
    matter, because x==0 can't be true. Hence, there is no failing
    path. */
	}
	return 0;
}
