#pragma expect_return()
#pragma no_other_results

int main() {
	struct s { char c; int *p; } s = {0, 0};
	free(s.p);
	return 0;
}
