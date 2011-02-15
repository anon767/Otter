#pragma expect_return()
#pragma no_other_results

struct s { char *p; };

int main() {
	struct s s1 = {"hello"}, *s2 = malloc(sizeof(s1));
	char *ps1 = &s1, *ps2 = (char*)s2;
	int n = sizeof(s1);
	while (n--) {
		*ps2++ = *ps1++;
	}
	__EVAL(s1);
	__EVAL(*s2);
	__ASSERT(s2->p[0] == 'h');
	return 0;
}
