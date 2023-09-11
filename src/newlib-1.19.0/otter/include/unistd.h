#ifndef __OTTER__UNISTD_H_
#define __OTTER__UNISTD_H_

#include_next <unistd.h>

// The declaration of gethostid in newlib's unistd.h is guarded by 
// __CYGWIN__
long gethostid(void);

#endif
