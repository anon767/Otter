#include <stddef.h>

int main()
{
	size_t i = sizeof(wchar_t);
	__ASSERT(i == 4);

	char *a = "0123456789";

	char *p = a + 5;
	char *q = a + 6;
	ptrdiff_t x = q - p;
	p = p + x;
	__ASSERT(*p == '6');

	return (0);
}
