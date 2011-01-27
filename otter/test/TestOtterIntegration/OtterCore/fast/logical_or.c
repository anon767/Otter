#pragma no_other_abandoned

int main() {
	int x;
	__SYMBOLIC(&x);
	if (x || !x) return 0;
	__ASSERT(0);
	return 0;
}
