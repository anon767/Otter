/* This tests makes sure that memset does a bounds-check: if p points to b, the
	 memset overruns the buffer. */
#pragma expect_abandoned(out_of_bounds)
#pragma expect_return()
#pragma no_other_results

int main() {
	char *a = malloc(5), *b = malloc(1), x, *p;
	__SYMBOLIC(&x);
	p = x ? a : b;
	memset(p, 1, sizeof(a));
	__ASSERT((x ? a[0] : b[0]) == 1);
	return 0;
}
