#pragma no_other_abandoned

#include <setjmp.h>

int i;
jmp_buf ev;

int bar(int i)
{
	longjmp(ev, 1);
	return (i);
}

void foo()
{
	i = bar(3);
}

int main()
{
	i = 0;
	if (setjmp(ev))
	{
		__ASSERT(i == 0);
		return (0);
	}
	else
		foo();

	__ASSERT(0); /* should jump away from this */
	return (0);
}
