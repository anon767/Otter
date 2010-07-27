#pragma no_other_abandoned

#include <setjmp.h>

int main()
{
	int i = __SYMBOLIC();

	jmp_buf ev[2];
	if (setjmp(ev[0]))
		i = 0;
	else
	{
		if(setjmp(ev[1]))
			i = 0;
		else
			longjmp(ev[__SYMBOLIC()%2], 1);
	}

	__ASSERT(i == 0);
	return (0);
}
