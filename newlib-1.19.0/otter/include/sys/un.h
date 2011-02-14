#ifndef _SYS_UN_H
#define _SYS_UN_H

#include <sys/socket.h>

/* http://www.unix.org/single_unix_specification_v3 says:

	 The size of sun_path has intentionally been left undefined. This is because
different implementations use different sizes. For example, 4.3 BSD uses a size
of 108, and 4.4 BSD uses a size of 104. Since most implementations originate
from BSD versions, the size is typically in the range 92 to 108. */

#define SUN_PATH_SIZE 92

struct sockaddr_un {
	sa_family_t  sun_family;
	char         sun_path[SUN_PATH_SIZE];
};

#endif
