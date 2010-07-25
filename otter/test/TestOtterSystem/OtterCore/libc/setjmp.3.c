#include <setjmp.h>

int i;

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
	jmp_buf ev;
	if (setjmp(ev))
		__ASSERT(i == 0);
		return (0);
	else
		foo();
	
	__ASSERT(0); /* should jump away from this */
	return (0);
}
