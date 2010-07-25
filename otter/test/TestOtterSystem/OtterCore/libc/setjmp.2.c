#include <setjmp.h>

void foo(jmp_buf ev)
{
	longjmp(ev, 1);
}

int main()
{
	jmp_buf ev;
	int i = setjmp(ev);
	if (i)
		return (0);
	else
		foo(ev);
	
	__ASSERT(0); /* should jump away from this */
	return (0);
}
