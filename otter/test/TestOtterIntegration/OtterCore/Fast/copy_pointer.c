/* As of r11070, Otter crashes on this because the memcpy mangles the
   pointer badly enough that Otter can't tell it's a pointer
   anymore. */
#pragma expect_return()
#pragma no_other_results

void *memcpy(void *dst, const void *src, unsigned int n)
{
	const char *p = src;
	char *q = dst;
	while (n--) {
		*q++ = *p++;
	}
	return dst;
}

struct s { char *p; };

int main() {
	struct s s1 = {"hello"}, *s2 = malloc(sizeof(s1));
	memcpy(s2, &s1, sizeof(s1));
	__EVAL(s1);
	__EVAL(*s2);
	__ASSERT(s2->p[0] == 'h');
	return 0;
}
