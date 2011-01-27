#pragma no_other_abandoned

int main() {
	long long x = 1LL << (8 * sizeof(int));
	_Bool b = x;
	__ASSERT(b);
	return 0;
}
