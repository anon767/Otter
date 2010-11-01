#pragma expect_return(a == 10)
#pragma expect_return(a == 100)
#pragma no_other_abandoned

int a = 5;

int main(char** argc, int argv)
{
	int pid = __otter_multi_fork();

	if (pid == 0)
	{
		a = 10;
		__ASSERT(a == 10);
	}
	else
	{
		a = 100;
		__ASSERT(a == 100);
	}

	return 0;
}
