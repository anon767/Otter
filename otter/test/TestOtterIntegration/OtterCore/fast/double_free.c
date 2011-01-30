#pragma expect_abandoned(failure("Double-free"))
#pragma no_other_results

int main() {
	void *p = malloc(1);
	free(p);
	free(p);
	return 0;
}
