#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned

void __SYMBOLIC(void *);

int main(char** argc, int argv)
{
	_Bool b;
	__SYMBOLIC(&b);
	if (b)
	{
		__otter_multi_fork();
	}
	else
	{
	}

	return 0;
}
