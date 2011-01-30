#pragma expect_abandoned(failure("Freeing a non-malloced pointer"))
#pragma no_other_results

int main() {
	char a[5], *p = a;
	free(p);
	return 0;
}
