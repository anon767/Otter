#include <sys/types.h>
#include <pwd.h>
#include <stdlib.h>
#include <symtest_mock.h>

struct passwd getpwnam_return  ;
struct passwd getpwuid_return  ;

#if __HAVE_getpwnam__
struct passwd *getpwnam(const char *name){
	getpwnam_return.pw_name = name;
	getpwnam_return.pw_uid = SERVER_UID;
	getpwnam_return.pw_gid = SERVER_GID;
	return &getpwnam_return;
}
#endif

#if __HAVE_endpwent__
void endpwent(void){
	// close the user database
}
#endif

#if __HAVE_getpwuid__
struct passwd *getpwuid(uid_t uid){
	getpwuid_return.pw_name = "somebody";
	getpwuid_return.pw_uid = SERVER_UID;
	getpwuid_return.pw_gid = SERVER_GID;
	return &getpwuid_return;
}
#endif
