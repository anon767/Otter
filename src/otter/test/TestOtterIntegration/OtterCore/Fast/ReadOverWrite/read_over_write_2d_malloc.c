#pragma no_other_abandoned

void __SYMBOLIC(void *);
void __ASSUME(_Bool);
void __ASSERT(_Bool);

typedef unsigned long size_t;
void * malloc(size_t size);

int main(char** argc, int argv)
{
	int** a = malloc(sizeof(int*)*2);

	int v;
	for (v = 0; v < 2; v++)
	{
		a[v] = malloc(sizeof(int)*4);
	}

	unsigned int i, j;
	__SYMBOLIC(&i);
	__ASSUME(i < 2);
	__SYMBOLIC(&j);
	__ASSUME(j < 4);

	int* b = a[i];

	b[0] = 0;

	__ASSERT(a[i][0] == 0);

	b[j] = 1;

	__ASSERT(a[i][j] == 1);
	
	return 0;
}
