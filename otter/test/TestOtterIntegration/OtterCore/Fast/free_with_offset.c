#pragma expect_abandoned(failure("Freeing a non-malloced pointer"))
#pragma no_other_results

typedef unsigned long size_t;
void * malloc(size_t size);
void free(void *);

int main() {
	char *p = malloc(2);
	free(p+1);
	return 0;
}
