#include <stdlib.h>

int main()
{
	__ASSERT(atoi("12345") == 12345);
	__ASSERT(atoi("   12345abcdef") == 12345);
	__ASSERT(atoi("0x1345") == 0);
	__ASSERT(atoi(" ") == 0);


	__ASSERT(atol("12345") == 12345);
	__ASSERT(atol("   12345abcdef") == 12345);
	__ASSERT(atol("0x1345") == 0);
	__ASSERT(atol(" ") == 0);

	__ASSERT(atoll("12345") == 12345);
	__ASSERT(atoll("   12345abcdef") == 12345);
	__ASSERT(atoll("0x1345") == 0);
	__ASSERT(atoll(" ") == 0);

	char** end = malloc(sizeof(char*));
	__ASSERT(strtoull("12345", end, 0) == 12345);
	__ASSERT(*(*end) == 0);
	__ASSERT(strtoull("11001010001110", end, 2) == 12942);
	__ASSERT(*(*end) == 0);
	__ASSERT(strtoull("11002010001110", end, 2) == 12);
	__ASSERT(*(*end) == '2');
	__ASSERT(strtoull("0001210", end, 3) == 48);
	__ASSERT(*(*end) == 0);
	__ASSERT(strtoull("0x001210", end, 3) == 0);
	__ASSERT(*(*end) == 'x');
	__ASSERT(strtoull("2E82CA82F4DE7380", end, 16) == 3351463736519848832ULL);
	__ASSERT(*(*end) == 0);
	__ASSERT(strtoull("02E82CA82F4DE7380", end, 16) == 3351463736519848832ULL);
	__ASSERT(*(*end) == 0);
	__ASSERT(strtoull("0x2E82CA82F4DE7380", end, 16) == 3351463736519848832ULL);
	__ASSERT(*(*end) == 0);
	__ASSERT(strtoull("0x6", end, 35) == 1161);
	__ASSERT(*(*end) == 0);
	__ASSERT(strtoull("0ZZ", end, 35) == 0);
	__ASSERT(*(*end) == 'Z');
	__ASSERT(strtoull("large", end, 35) == 31975524);
	__ASSERT(*(*end) == 0);

	return(0);
}
