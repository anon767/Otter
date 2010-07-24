#include <limits.h>

int main()
{
	char c = 1;

	__ASSERT((char) (c << (CHAR_BIT - 1)));
	__ASSERT(!((char) (c << CHAR_BIT)));

	__ASSERT(((signed char) (SCHAR_MAX + 1)) < (signed char) SCHAR_MAX);
	__ASSERT(((signed char) (SCHAR_MIN - 1)) > (signed char) SCHAR_MIN);

	__ASSERT(((unsigned char) (UCHAR_MAX + 1)) < (unsigned char) UCHAR_MAX);

	__ASSERT(((char) (CHAR_MAX + 1)) < (char) CHAR_MAX);
	__ASSERT(((char) (CHAR_MIN - 1)) > (char) CHAR_MIN);

	__ASSERT(((short) (SHRT_MAX + 1)) < (short) SHRT_MAX);
	__ASSERT(((short) (SHRT_MIN - 1)) > (short) SHRT_MIN);

	__ASSERT(((unsigned short) (USHRT_MAX + 1)) < (unsigned short) USHRT_MAX);

	__ASSERT(((int) (INT_MAX + 1)) < (int) INT_MAX);
	__ASSERT(((int) (INT_MIN - 1)) > (int) INT_MIN);

	__ASSERT(((unsigned int) (UINT_MAX + 1)) < (unsigned int) UINT_MAX);

	__ASSERT(((long) (LONG_MAX + 1)) < (long) LONG_MAX);
	__ASSERT(((long) (LONG_MIN - 1)) > (long) LONG_MIN);

	__ASSERT(((unsigned long) (ULONG_MAX + 1)) < (unsigned long) ULONG_MAX);

	return (0);
}
