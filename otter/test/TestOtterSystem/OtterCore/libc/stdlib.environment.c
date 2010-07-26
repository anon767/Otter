#include <stdlib.h>

int main()
{
	__ASSERT(getenv("ARGH") == 0);
	__ASSERT(system(0) == 0);
	__ASSERT(system("asd") == 1);
}
