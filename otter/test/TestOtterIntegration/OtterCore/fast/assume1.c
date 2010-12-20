#pragma no_other_results
// The __ASSUME should eliminate the only path.

int main() {
	int x;
	__SYMBOLIC(&x);
	__ASSUME(x < 0, x > 0);
	return 0;
}
