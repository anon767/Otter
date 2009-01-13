#include "iosim.h"
#include <unistd.h>
#include <string.h>


#if __HAVE_getpid__
pid_t        getpid(void){
	//__LIMIT_FUNC(getpid,1);
	return (pid_t) 1;
}
#endif

#if __HAVE_getgid__
gid_t        getgid(void){ 
	return (gid_t) SERVER_GID;
}
#endif

#if __HAVE_fork__
pid_t fork(void){
	return 1; // child process id > 0
}
#endif

#if __HAVE_getuid__
uid_t        getuid(void){
	return (uid_t) SERVER_UID;
}
#endif

#if __HAVE_gethostname__
int gethostname(char *name, size_t namelen){
	strcpy(name,SERVER_IP);
	return 0;
}
#endif

#if __HAVE_pipe__
int pipe(int fildes[2]){
	fildes[0] = IOSIM_newfd(); // 6
	fildes[1] = IOSIM_newfd(); // 7, never use coz we don't really fork a child
	return 0;
}
#endif


#if __HAVE_close__
int close(int fildes){
	return 0;
}
#endif

