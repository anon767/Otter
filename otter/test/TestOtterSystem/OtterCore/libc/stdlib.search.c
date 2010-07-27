#pragma no_other_abandoned

#include <stdlib.h>

int ccomp(const void* a, const void* b)
{
	char ca = *((char*)(a));
	char cb = *((char*)(b));
	if(ca < cb)
		return(-1);
	else if (ca > cb)
		return(1);
	else
		return(0);
}

int icomp(const void* a, const void* b)
{
	int ia = *((int*)(a));
	int ib = *((int*)(b));
	if(ia < ib)
		return(-1);
	else if (ia > ib)
		return(1);
	else
		return(0);
}

int main()
{
	char* a = "0123456789";
	char c = '5';
	__ASSERT(*((char*)bsearch(&c, a, 10, sizeof(char), ccomp)) == '5');
	c = 'x';
	__ASSERT(((char*)bsearch(&c, a, 10, sizeof(char), ccomp)) == 0);

	int b[] = {0, 10, 20, 30, 40, 50, 60, 70, 80, 90};
	int i = 70;
	__ASSERT(*((int*)bsearch(&i, b, 10, sizeof(int), icomp)) == 70);
	i = 33;
	__ASSERT(((int*)bsearch(&i, b, 10, sizeof(int), icomp)) == 0);

	return(0);
}
