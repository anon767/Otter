#ifndef __OTTER_USER_H
#define __OTTER_USER_H

#include <sys/types.h>

#define __otter_UID_ROOT 0
#define __otter_UID_USER 1
#define __otter_UID_INVALID -1

#define __otter_GID_ROOT 0
#define __otter_GID_USER 1
#define __otter_GID_INVALID -1

uid_t __otter_uid = __otter_UID_USER;
gid_t __otter_gid = __otter_GID_USER;

#endif
