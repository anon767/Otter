/* ctype.c: Predicates on characters. */

int __otter_libc_iscntrl(int c)
{
	return (c >= 0x00 & c <= 0x1f) || (c == 0x7f);
}

int __otter_libc_isdigit(int c)
{
	return (c >= '0' & c <= '9');
}

int __otter_libc_isgraph(int c)
{
	return (c >= 0x21 & c <= 0x7e);
}

int __otter_libc_islower(int c)
{
	return (c >= 'a' & c <= 'z');
}

int __otter_libc_isprint(int c)
{
	return (c >=0x20 & c <= 0x7e);
}

int __otter_libc_isspace(int c)
{
	return (c == ' ' | c == '\r' | c == '\t' | c == '\n' | c == '\f' | c == '\v');
}

int __otter_libc_isupper(int c)
{
	return (c >= 'A' & c <= 'Z');
}

int __otter_libc_isxdigit(int c)
{
	return ((c >= '0' & c <= '9') | (c >= 'A' & c <= 'F') | (c >= 'a' & c <= 'f'));
}

int __otter_libc_isalpha(int c)
{
	return isupper(c) | islower(c);
}

int __otter_libc_isalnum(int c)
{
	return isalpha(c) | isdigit(c);
}

int __otter_libc_ispunct(int c)
{
	return !isalnum(c) & isgraph(c);
}

int __otter_libc_tolower(int c)
{
	return isupper(c) ? c ^ 0x20 : c;
}

int __otter_libc_toupper(int c)
{
	return islower(c) ? c ^ 0x20 : c;
}


