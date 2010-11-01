#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned

int foo(int b) {return b;}
int bar(int c) {return c;}

int main(char** argc, int argv)
{
	int i = 0;
	__otter_multi_fork();
	i++;
	__otter_multi_fork();
	i++;
	__otter_multi_fork();
	__ASSERT(i == 2);

	return(0);
}
