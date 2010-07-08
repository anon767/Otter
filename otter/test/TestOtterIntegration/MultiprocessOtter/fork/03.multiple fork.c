int foo(int b) {return b;}
int bar(int c) {return c;}

int main(char** argc, int argv)
{
	int i = 0;
	fork();
	i++;
	fork();
	i++;
	fork();
	__ASSERT(i == 2);

	return(0);
}
