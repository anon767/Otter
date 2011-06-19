#pragma no_other_abandoned

#include <setjmp.h>

int main()
{
	int i;
	__SYMBOLIC(&i);

	jmp_buf ev[2];
	if (setjmp(ev[0]))
		i = 0;
	else
	{
		if(setjmp(ev[1]))
			i = 0;
		else {
			unsigned k;
			__SYMBOLIC(&k);
			longjmp(ev[k%2], 1);
		}
	}

	__ASSERT(i == 0);
	return (0);
}
