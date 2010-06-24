

int main(char** argc, int argv)
{
	int a = 5;
	int pid = fork();

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
