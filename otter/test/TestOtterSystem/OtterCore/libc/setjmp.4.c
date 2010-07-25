#include <setjmp.h>

int i;

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
	jmp_buf ev;
	if (setjmp(ev))
		i = 0;
	else
		foo();
	
	__ASSERT(i == 0);
	return (0);
}
