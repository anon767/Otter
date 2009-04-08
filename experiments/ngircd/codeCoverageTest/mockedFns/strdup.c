#include <string.h>
#include <stdlib.h>

char* strdup(const char* p_str)
{
	size_t len = strlen(p_str);
	char* theCopy = malloc(len + 1); // '+ 1' for the null terminator
	if (theCopy) {
		strcpy(theCopy,p_str);
	}
	return theCopy;
}
