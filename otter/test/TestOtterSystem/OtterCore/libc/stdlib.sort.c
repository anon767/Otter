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
	char a[] = {'1', '7', '4', '0', '2', '8', '3', '6', '5', '9'};
	qsort(a, 10, sizeof(char), ccomp);
	for(int i = 0; i < 10; i++)
	{
		__ASSERT(a[i] == '0' + i);
	}

	int b[] = {10, 70, 40, 0, 20, 80, 30, 60, 50, 90};
	qsort(b, 10, sizeof(int), icomp);
	for(int i = 0; i < 10; i++)
	{
		__ASSERT(b[i] == i * 10);
	}

	return(0);
}
