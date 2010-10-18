#ifndef _ARPA_INET_H
#define _ARPA_INET_H

#include <netinet/in.h>

#define htonl(x) (x)
#define htons(x) (x)
#define ntohl(x) (x)
#define ntohl(x) (x)

in_addr_t      inet_addr(const char *cp);
in_addr_t      inet_lnaof(struct in_addr in);
struct in_addr inet_makeaddr(in_addr_t net, in_addr_t lna);
in_addr_t      inet_netof(struct in_addr in);
in_addr_t      inet_network(const char *cp);
char          *inet_ntoa(struct in_addr in);
int            inet_aton(const char *cp, struct in_addr *addrptr);

#endif
