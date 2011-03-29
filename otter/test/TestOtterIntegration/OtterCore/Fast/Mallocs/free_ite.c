#pragma expect_abandoned(failure("Dereference"))
#pragma no_other_results

typedef unsigned long size_t;
void * malloc(size_t size);
void free(void *);

int main() {
	int x;
	__SYMBOLIC(&x);
	char *a = malloc(1), *b = malloc(2), *c = x ? a : b;
	free(c);
	*c = 0;
	return 0;
}
