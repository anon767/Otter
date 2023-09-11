#pragma expect_return()
#pragma no_other_results

int main() {
	struct s { int *p; char c; } s = {0, 0};
	free(s.p);
	return 0;
}
