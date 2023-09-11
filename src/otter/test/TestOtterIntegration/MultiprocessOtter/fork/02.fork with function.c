#pragma expect_return(b == 10)
#pragma expect_return(b == 100)
#pragma no_other_abandoned

int foo(int b) {return b;}
int bar(int c) {return c;}

int b = 0;

int main(char** argc, int argv)
{
	int a = 5;
	int pid = __otter_multi_fork();

	if (pid == 0)
	{
		a = 10;
		b = foo(a);
		__ASSERT(b == 10);
	}
	else
	{
		a = 100;
		b = foo(a);
		__ASSERT(b == 100);
	}

	return 0;
}
