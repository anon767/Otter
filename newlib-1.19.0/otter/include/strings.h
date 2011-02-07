#ifndef _STRINGS_H
#define _STRINGS_H

// See http://www.opengroup.org/onlinepubs/009695399/functions/bzero.html
#define bzero(b,len) ((void)memset((b), 0, (len)))

int strcasecmp(const char *s1, const char *s2);
int strncasecmp(const char *s1, const char *s2, size_t n);

#endif
