// If c == a, fail
#pragma expect_abandoned(failure("Dereference"))

// If c == b, succeed
#pragma expect_return()

#pragma no_other_results

int main() {
	int x;
	__SYMBOLIC(&x);
	char *a = malloc(1), *b = malloc(2), *c = x ? a : b;
	free(c);
	*a = 0;
	return 0;
}
