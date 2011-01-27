int main() {
	int x;
	__SYMBOLIC(&x);
	if (x > 0 && x < 0) __ASSERT(0);
	return 0;
}
