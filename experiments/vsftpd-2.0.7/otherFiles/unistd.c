#include <iosim.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>


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

//#if __HAVE_fork__
pid_t fork(void){
//	static pid_t new_pid = 0;
//	if ((char)__SYMBOLIC()) {
//		__EVALSTR("Forking to parent",17);
//		new_pid++;
//		return new_pid; // child process id > 0
//	}
	__EVALSTR("Forking to child",16);
	return 0;
}
//#endif

//#if __HAVE_getuid__
uid_t        getuid(void){
	return (uid_t) 0; // vsftpd must run as root
}
//#endif

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

//#if __HAVE_read__
ssize_t read(int fildes, void *buf, size_t nbyte){
	return IOSIM_read(fildes, buf, nbyte);
}	
//#endif

//#if __HAVE_close__
int close(int fildes){
	IOSIM_close(fildes);
	return 0;
}
//#endif

//char write_output[10][1000];
ssize_t write(int fildes, const void *buf, size_t nbyte){
//	static int call_count = 0;
//
//	call_count ++;
//	switch(call_count){
//	case 1:
//	case 2:
//	case 3:
//	case 4:
//	default: 
		//assert(fildes==5);
		//strncat(write_output[fildes],buf,nbyte);
		//__EVALSTR(write_output[fildes]);
//	switch (fildes) {
//	case 0: __EVALSTR("Writing on fildes 0",20); break;
//	case 1: __EVALSTR("Writing on fildes 1",20); break;
//	case 2: __EVALSTR("Writing on fildes 2",20); break;
//	case 3: __EVALSTR("Writing on fildes 3",20); break;
//	case 4: __EVALSTR("Writing on fildes 4",20); break;
//	case 5: __EVALSTR("Writing on fildes 5",20); break;
//	default: __EVALSTR("Writing on fildes >5",20); break;
//	}
		__EVALSTR(buf,nbyte);
		return nbyte;
//	}
//	return -1;
}

pid_t setsid() {
	return 0;
}
pid_t getpid() {
	return 0;
}
pid_t getpgrp() {
	return 0;
}
int setgid(gid_t gid) {
	return 0;
}
int setuid(uid_t uid) {
	return 0;
}
char *getcwd(char *buf, size_t size) {
	strcpy(buf, "fake/cwd/");
	return buf;
}
int dup2(int fildes, int fildes2) {
	return fildes2;
}
