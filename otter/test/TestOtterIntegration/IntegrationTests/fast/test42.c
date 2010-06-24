int foo(int b) {return b;}
int bar(int c) {return c;}

int main(char** argc, int argv)
{
	int a = 5;
	int pid = fork();

	if (pid == 0)
	{
		a = 10;
		__ASSERT(foo(a) == 10);
	}
	else
	{
		a = 100;
		__ASSERT(foo(a) == 100);
	}

	return 0;
}
