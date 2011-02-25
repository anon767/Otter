#ifndef _ARPA_INET_H
#define _ARPA_INET_H

#include <netinet/in.h>

/* byte reordering macros */
#define htonl(x) ((((x) & 0xFF000000) >> 24) | (((x) & 0x00FF0000) >> 8) | (((x) & 0x0000FF00) << 8) | (((x) & 0x000000FF) << 24))
#define htons(x) ((((x) & 0xFF00) >> 8) | (((x) & 0x00FF) << 8))
#define ntohl(x) ((((x) & 0xFF000000) >> 24) | (((x) & 0x00FF0000) >> 8) | (((x) & 0x0000FF00) << 8) | (((x) & 0x000000FF) << 24))
#define ntohs(x) ((((x) & 0xFF00) >> 8) | (((x) & 0x00FF) << 8))

in_addr_t      inet_addr(const char *cp);
in_addr_t      inet_lnaof(struct in_addr in);
struct in_addr inet_makeaddr(in_addr_t net, in_addr_t lna);
in_addr_t      inet_netof(struct in_addr in);
in_addr_t      inet_network(const char *cp);
char          *inet_ntoa(struct in_addr in);
int            inet_aton(const char *cp, struct in_addr *addrptr);

#endif
