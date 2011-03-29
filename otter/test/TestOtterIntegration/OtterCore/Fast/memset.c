#pragma expect_return()
#pragma no_other_results

typedef unsigned long size_t;
void * memset(void *b, int c, size_t len);

int main() {
	char a[5], b[5], x, *p;
	__SYMBOLIC(&x);
	p = x ? &a : &b;
	memset(p, 1, sizeof(a));
	__ASSERT((x ? a[1] : b[4]) == 1);
	return 0;
}
