#pragma expect_abandoned(failure("Dereference a dangling pointer"))
#pragma no_other_results

int main() {
	int x;
	__SYMBOLIC(&x);
	char *a = malloc(1), *b = malloc(2), *c = x ? a : b;
	free(c);
	*c = 0;
	return 0;
}
