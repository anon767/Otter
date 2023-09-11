#pragma expect_abandoned(failure("Write to a const"))
#pragma no_other_results

void f(const int *p) {
	int *q = p;
	*q = 0;
}
int main() {
	int x = 5;
	f(&x);
	return 0;
}
