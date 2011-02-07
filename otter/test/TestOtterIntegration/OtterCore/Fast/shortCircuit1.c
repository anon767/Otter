#pragma no_other_abandoned

int a[10];
int main() {
	unsigned int i;
	__SYMBOLIC(&i);
	__ASSERT(i >= 10 || a[i] == 0);
	return 0;
}
