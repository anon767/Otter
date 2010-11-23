#ifndef STRINGS_H
#define STRINGS_H

// See http://www.opengroup.org/onlinepubs/009695399/functions/bzero.html
#define bzero(b,len) ((void)memset((b), 0, (len)))

#endif
