#include <stdarg.h>

int is_sorted(int arg0, ...)
{
	va_list va;
	va_start(va, arg0);

	int i = arg0;
	int j = -1;
	int r = 1;
	
	while(i > -1)
	{
		r = r & (i > j);
		j = i;
		i = va_arg(va, int);
	}

	va_end(va);

	return r;
}

int main()
{
	__ASSERT(is_sorted(-1));
	__ASSERT(is_sorted(0, 1, 2, 3, -1));
	__ASSERT(!is_sorted(12, 45, 12, 45, 12, 45, 12, 45, 12, 45, 12, 45, -1));

	int i = (__SYMBOLIC() % 9) + 1;

	__ASSERT(is_sorted(0, i, 101, 999, i*1000, -1));
	return (0);
}
