

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
