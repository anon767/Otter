#include <grp.h>
#include <stdlib.h>

#if __HAVE_getgrnam__
struct group getgrnam_return;
struct group *getgrnam(const char* name){
	getgrnam_return.gr_name = "group";
	getgrnam_return.gr_gid = SERVER_GID;
	getgrnam_return.gr_mem = "";
	return &getgrnam_return;
}
#endif

#if __HAVE_getgrgid__
struct group getgrgid_return;
struct group *getgrgid(gid_t gid){
	getgrgid_return.gr_name = "group";
	getgrgid_return.gr_gid = SERVER_GID;
	getgrgid_return.gr_mem = "";
	return &getgrgid_return;
}
#endif
