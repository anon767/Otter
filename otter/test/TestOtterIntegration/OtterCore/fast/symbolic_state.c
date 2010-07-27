#pragma no_other_abandoned

typedef unsigned long size_t;
void * malloc(size_t size);
void * memset(void *b, int c, size_t len);

typedef struct __s{
	int a;
	char arr[10];
} S;

S global;

int main(){

	S local;
	S* heap = malloc (sizeof(S));

	__ASSERT(global.arr[5] == 0);
	// local and heap memories are initially undefined

	memset(&global,1,sizeof(S));
	memset(&local,1,sizeof(S));
	memset(heap,1,sizeof(S));

	//__TRUTH_VALUE(x)==
	//		1 -> x is true (non 0)
	//		0 -> x is unknown (symbolic)
	//		-1 -> x is false (0)
	__ASSERT(__TRUTH_VALUE(global.arr[5]) == 1);
	__ASSERT(__TRUTH_VALUE(local.arr[5]) == 1);
	__ASSERT(__TRUTH_VALUE(heap->arr[5]) == 1);

	__ASSERT(global.arr[5] == 1);
	__ASSERT(local.arr[5] == 1);
	__ASSERT(heap->arr[5] == 1);

	__SYMBOLIC_STATE();

	__ASSERT(__TRUTH_VALUE(global.arr[5]) == 0);
	__ASSERT(__TRUTH_VALUE(local.arr[5]) == 0);
	__ASSERT(__TRUTH_VALUE(heap == 0) == 0);

	return 0;
}
