
int main(char** argc, int argv)
{
	int** a = malloc(sizeof(int*)*2);

	int v;
	for (v = 0; v < 2; v++)
	{
		a[v] = malloc(sizeof(int)*4);
	}

	int i = __SYMBOLIC()%2;
	int j = __SYMBOLIC()%4;

	int* b = a[i];

	b[0] = 0;

	__ASSERT(a[i][0] == 0);

	b[j] = 1;

	__ASSERT(a[i][j] == 1);
	
	return 0;
}
