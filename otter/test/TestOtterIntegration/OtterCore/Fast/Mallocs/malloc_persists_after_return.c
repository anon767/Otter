#pragma no_other_abandoned

typedef unsigned long size_t;
void * malloc(size_t size);

int* foo() 
{ 
	return malloc(sizeof(int));
}

int main ()
{
	int *i = foo();
	*i = 12;
	__ASSERT(*i == 12);

	return(0);
}
