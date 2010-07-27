#pragma no_other_abandoned

#include <setjmp.h>

int main()
{
	jmp_buf ev;
	if (setjmp(ev))
		return (0);
	else
		longjmp(ev, 1);

	__ASSERT(0); /* should jump away from this */
	return (0);
}
