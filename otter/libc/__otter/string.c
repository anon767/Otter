#include <string.h>

void* __otter_libc_memcpy(void* s1, const void* s2, size_t n)
{
	if(s1 && s2 && n)
	{
		for(int i = 0; i < n; i++)
		{
			((char*)s1)[i] = ((char*)s2)[i];
		}
	}

	return (s1);
}

void* __otter_libc_memmove(void* s1, const void* s2, size_t n)
{
	if(s1 && s2 && n)
	{
		char* tmp = malloc(n);
		for(int i = 0; i < n; i++)
		{
			tmp[i] = ((char*)s2)[i];
		}
		for(int i = 0; i < n; i++)
		{
			((char*)s1)[i] = tmp[i];
		}
		free(tmp);
	}

	return (s1);
}

char* __otter_libc_strcpy(char* s1, const char* s2)
{
	if(s1 && s2)
	{
		for(int i = 0; ; i++)
		{
			s1[i] = s2[i];
			if(s1[i] == 0)
				return (s1);
		}
	}

	return (s1);
}

char* __otter_libc_strncpy(char* s1, const char* s2, size_t n)
{
	if(s1 && s2 && n)
	{
		for(int i = 0; i < n; i++)
		{
			s1[i] = s2[i];
			if(s1[i] == 0)
				return (s1);
		}
	}

	return (s1);
}

char* __otter_libc_strcat(char* s1, const char* s2)
{
	if(s1 && s2)
	{
		int i = 0;
		while(s1[i] != 0)
			i++;

		for(int j = 0; ; j++, i++)
		{
			s1[i] = s2[j];
			if(s2[j] == 0)
				return (s1);
		}
	}

	return (s1);
}

char* __otter_libc_strncat(char* s1, const char* s2, size_t n)
{
	if(s1 && s2 && n)
	{
		int i = 0;
		while(s1[i] != 0)
			i++;

		for(int j = 0; j < n; j++, i++)
		{
			s1[i] = s2[j];
			if(s2[j] == 0)
				return (s1);
		}
	}

	return (s1);
}

int __otter_libc_memcmp(const void* s1, const void* s2, size_t n)
{
	if(s1 && s2 && n)
	{
		for(int i = 0; i < n; i++)
		{
			if (((char*)s1)[i] > ((char*)s2)[i])
				return (1);
			else if (((char*)s1)[i] < ((char*)s2)[i])
				return (-1);
		}
	}

	return (0);
}

int __otter_libc_strcmp(const char* s1, const char* s2)
{
	if(s1 && s2)
	{
		for(int i = 0; ; i++)
		{
			if (((char*)s1)[i] > ((char*)s2)[i])
				return (1);
			else if (((char*)s1)[i] < ((char*)s2)[i])
				return (-1);

			if(((char*)s1)[i] == 0)
				return 0;
		}
	}

	return (0);
}

/* locale is basically not used, so strcoll does nothing extra */
int __otter_libc_strcoll(const char* s1, const char* s2)
{
	return strcmp(s1, s2);
}

/* Grabbed from newlib */
int __otter_libc_strncmp(const char* s1, const char* s2, size_t n)
{
  if (n == 0)
    return 0;

  while (n-- != 0 && *s1 == *s2)
    {
      if (n == 0 || *s1 == '\0')
        break;
      s1++;
      s2++;
    }

  return (*(unsigned char *) s1) - (*(unsigned char *) s2);
}

/* similarly strxfrm is basically srtncpy */
int __otter_libc_strxfrm(char* s1, const char* s2, int n)
{
	if(s2)
	{
		int i = 0;
		while(s1[i] != 0)
			i++;

		if(n == 0 || i > n)
		{
			return (i);
		}

		if(s1)
		{
			strncpy(s1, s2, n);
			return (i);
		}
	}

	return (0);
}

void* __otter_libc_memchr(const void* s, int c, size_t n)
{
	for(int i = 0; i < n; s++)
	{
		if(*((unsigned char*)s) == ((unsigned char)c))
			return s;
	}

	return (0);
}

char* __otter_libc_strchr(const char* s, int c)
{
	for(; ; s++)
	{
		if(*s == ((char)c))
			return s;
		if(*s == 0) /* return NULL here only if c != 0 */
			return 0;
	}

	return (0);
}

size_t __otter_libc_strcspn(const char* s1, const char* s2)
{
	int i = 0;
	for(; ; i++)
	{
		if(s1[i] == 0) /* got to the end of s1 */
			return(i);
		if(strchr(s2, s1[i])) /* search for s1[i] in s2 */
			return(i);
	}

	return (i);
}

char* __otter_libc_strpbrk(const char* s1, const char* s2)
{
	char *loc = 0;
	for(; ; s1++)
	{
		if(*s1 == 0) /* ran out of chars  from s1 to search for in s2 */
			return NULL;
		loc = strchr(s2, *s1); /* search for s1[i] in s2 */
		if(loc != NULL)
			return loc;
	}

	return NULL;
}

char* __otter_libc_strrchr(const char* s, int c)
{ // Taken (and slightly modified) from uClibc
	const char *p = NULL;
	do {
		if (*s == (char) c) {
			p = s;
		}
	} while (*s++);

	return (char *) p;
}

size_t __otter_libc_strspn(const char* s1, const char* s2)
{
	int i = 0;
	for(; ; i++)
	{
		if(s1[i] == 0) /* got to the end of s1 */
			return(i);
		if(!strchr(s2, s1[i])) /* search for s1[i] in s2 */
			return(i);
	}

	return (i);
}

int __otter_libc_isprefixstr(const char* s1, const char* s2)
{
	for(int i = 0; ; i++)
	{
		if(s1[i] != s2[i])
			return (0);
		if(s1[i] == 0) /* end of both strings */
			return (1);

	}

	return (0);
}

char* __otter_libc_strstr(const char* s1, const char* s2)
{
	for(; ; s1++)
	{
		if(__otter_libc_isprefixstr(s1, s2))
			return (s1);
	}

	return (0);
}

char* __otter_libc_strtok_param(char* s, const char* s2)
{
	s += strspn(s, s2); /* find end of seperator sequence */
	if(*s == 0) /* only seperators left */
		return (0);
	char* r = s;
	s += strcspn(s, s2); /* find end of token sequence for next call */
	*s = 0; /* terminate token (replace seperator with null character) */
	s++;
	return r; /* return start of token */

}

char* __otter_libc_strtok(char* s1, const char* s2)
{
	static char* s = 0;

	if(s1)
	{
		s = s1;
	}

	return __otter_libc_strtok_param(s, s2);
}

void* __otter_libc_memset(void* s, int c, size_t n)
{
	for(int i = 0; i < n; i++)
	{
		((unsigned char*)s)[i] = (unsigned char)c;
	}
	return s;
}

char* __otter_libc_strerror(int errnum)
{
	return "Error of some kind";
}

int __otter_libc_strlen(const char* s)
{
	int i = 0;
	while(s[i] != 0)
		i++;

	return (i);
}

char* __otter_libc_strdup(const char* s) {
	size_t len = strlen(s);
	char* theCopy = malloc(len + 1); // '+ 1' for the null terminator
	if (theCopy) {
		strcpy(theCopy, s);
	}
	return theCopy;
}

// Taken (and slightly modified) from uClibc
char *__otter_libc_stpcpy(char *dst, const char *src)
{
	while ( *dst++ = *src++ );
  return dst-1;
}
