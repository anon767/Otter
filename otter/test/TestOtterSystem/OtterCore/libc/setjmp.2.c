#include <setjmp.h>

void foo()
{
	longjmp(ev, 1);
}

int main()
{
	jmp_buf ev;
	if (setjmp(ev))
		return (0);
	else
		foo();
	
	__ASSERT(0); /* should jump away from this */
	return (0);
}
