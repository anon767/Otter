#pragma expect_abandoned("Dereference into an expired stack frame")

int* foo() 
{ 
	return __builtin_alloca(sizeof(int));
}

int main ()
{
	int *i = foo();
	*i = 12;
	__ASSERT(*i == 12);

	return(0);
}
