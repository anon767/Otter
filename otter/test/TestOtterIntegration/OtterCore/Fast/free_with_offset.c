#pragma expect_abandoned(failure("Freeing a non-malloced pointer"))
#pragma no_other_results

int main() {
	char *p = malloc(2);
	free(p+1);
	return 0;
}
