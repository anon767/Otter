#include <ctype.h>

int main()
{
	__ASSERT(isalnum('a'));
	__ASSERT(isalpha('b'));
	__ASSERT(iscntrl('\n'));
	__ASSERT(isdigit('5'));
	__ASSERT(isgraph('+'));
	__ASSERT(isprint('%'));
	__ASSERT(ispunct(';'));
	__ASSERT(isspace(' '));
	__ASSERT(isupper('M'));
	__ASSERT(isalnum('a'));
	__ASSERT(isxdigit('E'));
	__ASSERT(tolower('A') == 'a');
	__ASSERT(toupper('a') == 'A');
	__ASSERT(tolower('m') == 'm');
	__ASSERT(toupper('M') == 'M');

	return (0);
}
