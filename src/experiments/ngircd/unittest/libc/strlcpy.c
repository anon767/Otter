#include <string.h>
//#include <sys/types.h>

size_t
strlcat( char *dst, const char *src, size_t size )
{
	/* Like strncat() but does not 0 fill the buffer and
	 * always null terminates. */

	size_t len1 = strlen( dst );
	size_t len2 = strlen( src );
	size_t ret = len1 + len2;

	if( size && ( len1 < size - 1 )) {
		if( len2 >= size - len1 )
			len2 = size - len1 - 1;
		memcpy( dst + len1, src, len2 );
		dst[len1 + len2] = 0;
	}
	return ret;
} /* strlcat */


size_t
strlcpy( char *dst, const char *src, size_t size )
{
	/* Like strncpy but does not 0 fill the buffer and
	 * always null terminates. */

	size_t len = strlen( src );
	size_t ret = len;

	if( size > 0 ) {
		if( len >= size ) len = size - 1;
		memcpy( dst, src, len );
		dst[len] = 0;
	}
	return ret;
} /* strlcpy */

