#include <errno.h>

int main()
{
	__ASSERT(errno == 0);
	errno = EDOM;
	__ASSERT(errno == EDOM);
	errno = ERANGE;
	__ASSERT(errno == ERANGE);

	return (0);
}
