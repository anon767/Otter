#pragma no_other_abandoned

int x;
int *y = &x;
int x = 5;
int main() {
	__ASSERT(y == &x);
	return 0;
}
