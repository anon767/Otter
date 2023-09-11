#pragma expect_abandoned(failure("Double-free"))
#pragma no_other_results

typedef unsigned long size_t;
void * malloc(size_t size);
void free (void *);

int main() {
	void *p = malloc(1);
	free(p);
	free(p);
	return 0;
}
