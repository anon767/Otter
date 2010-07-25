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
			longjmp(ev[i%2]);
		else
			i = 0;
	}
	
	__ASSERT(i == 0);
	return (0);
}
