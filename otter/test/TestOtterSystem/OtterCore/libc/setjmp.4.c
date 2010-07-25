#include <setjmp.h>

int i;
jmp_buf ev;

void bar()
{
	if (i)
		longjmp(ev, 1);
}

void foo()
{
	bar();
}

int main()
{
	i = __SYMBOLIC();
	if (setjmp(ev))
		i = 0;
	else
		foo();
	
	__ASSERT(i == 0);
	return (0);
}
