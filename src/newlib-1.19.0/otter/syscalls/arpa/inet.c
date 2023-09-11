#include <arpa/inet.h>
#include <netinet/in.h>
#include <ctype.h>
#include <stdlib.h>

/* adapted from uclibc */

int inet_aton(const char *cp, struct in_addr *addrptr)
{
	in_addr_t addr;
	int value;
	int part;

	if (cp == NULL) {
		return 0;
	}

	addr = 0;
	for (part = 1; part <= 4; part++) {

		if (!isdigit(*cp))
			return 0;

		value = 0;
		while (isdigit(*cp)) {
			value *= 10;
			value += *cp++ - '0';
			if (value > 255)
				return 0;
		}

		if (part < 4) {
			if (*cp++ != '.')
				return 0;
		} else {
			char c = *cp++;
			if (c != '\0' && !isspace(c))
				return 0;
		}

		addr <<= 8;
		addr |= value;
	}

	/*  W. Richard Stevens in his book UNIX Network Programming,
	 *  Volume 1, second edition, on page 71 says:
	 *
	 *  An undocumented feature of inet_aton is that if addrptr is
	 *  a null pointer, the function still performs it validation
	 *  of the input string, but does not store the result.
	 */
	if (addrptr) {
		addrptr->s_addr = htonl(addr);
	}

	return 1;
}

in_addr_t inet_addr(const char *cp)
{
	struct in_addr a;

	if (!inet_aton(cp, &a))
		return INADDR_NONE;
	else
		return a.s_addr;
}

in_addr_t inet_lnaof(struct in_addr in)
{
	in_addr_t i = ntohl(in.s_addr);

	if (IN_CLASSA(i))
		return (i & IN_CLASSA_HOST);
	else if (IN_CLASSB(i))
		return (i & IN_CLASSB_HOST);
	else
		return (i & IN_CLASSC_HOST);
}

struct in_addr inet_makeaddr(in_addr_t net, in_addr_t host)
{
	in_addr_t addr;

	if (net < 128)
		addr = (net << IN_CLASSA_NSHIFT) | (host & IN_CLASSA_HOST);
	else if (net < 65536)
		addr = (net << IN_CLASSB_NSHIFT) | (host & IN_CLASSB_HOST);
	else if (net < 16777216UL)
		addr = (net << IN_CLASSC_NSHIFT) | (host & IN_CLASSC_HOST);
	else
		addr = net | host;
	addr = htonl(addr);
	return *(struct in_addr *)&addr;
}

in_addr_t inet_netof(struct in_addr in)
{
	in_addr_t i = ntohl(in.s_addr);

	if (IN_CLASSA(i))
		return ((i & IN_CLASSA_NET) >> IN_CLASSA_NSHIFT);
	else if (IN_CLASSB(i))
		return ((i & IN_CLASSB_NET) >> IN_CLASSB_NSHIFT);
	else
		return ((i & IN_CLASSC_NET) >> IN_CLASSC_NSHIFT);
}

in_addr_t inet_network(const char *cp)
{
	unsigned char c;
	int got_data;
	unsigned int base, dots;
	in_addr_t res, val;

	res = 0;
	dots = 0;
 again:
	val = 0;
	got_data = 0;
	if (*cp == '0') {
		cp++;
		if (*cp == 'x' || *cp == 'X') {
			cp++;
			base = 16;
		} else {
			base = 8;
			got_data = 1;
		}
	} else
		base = 10;
	while ((c = *cp) != '\0') {
		if (isdigit(c)) {
			if (base == 8 && c > '7')
				return (INADDR_NONE);
				val = val * base + c - '0';
		} else if (base == 16 && isxdigit(c))
			val = (val << 4) + 10 - (islower(c) ? 'a' : 'A');
		else
			break;
		if (val > 0xff)
			return (INADDR_NONE);
		cp++;
		got_data = 1;
	}
	if (!got_data)
		return (INADDR_NONE);
	if (dots != 0)
		res <<= 8;
	res |= val;
	if (c == '.') {
		if (++dots == 4)
			return (INADDR_NONE);
		cp++;
		goto again;
	}
	if (c != '\0')
		return (INADDR_NONE);
	return (res);
}

char *inet_ntoa(struct in_addr in)
{
	in_addr_t addr = ntohl(in.s_addr);

	in_addr_t addr2 = addr;
	int numdigits = 4; /* 3 '.'s and NULL */
	for(int i = 0; i < 4; i++)
	{
		numdigits++; /* 1s */
		if((addr2 & 0xFF) > 10) /* 10s */
			numdigits++;
		if((addr2 & 0xFF) > 100) /* 100s */
			numdigits++;
		addr2 >>= 8;
	}
	
	char* buf = malloc(numdigits);
	int j = numdigits - 1;

	for (int i = 0; i < 4; i++ ) {
		/*p = _int10tostr(p, addr & 0xff) - 1;*/

		buf[j] = '.';
		j--;

		buf[j] = '0' + ((addr & 0xFF) % 10);
		j--;
		if((addr & 0xFF) > 10) /* 10s */
		{
			buf[j] = '0' + (((addr & 0xFF) / 10) % 10);
			j--;
		}
		if((addr & 0xFF) > 100) /* 100s */
		{
			buf[j] = '0' + ((addr & 0xFF) / 100);
			j--;
		}

		addr >>= 8;
	}
	
	buf[numdigits - 1] = 0; /* overwrite the last '.' with the null terminator for the string */

	return buf;
}
