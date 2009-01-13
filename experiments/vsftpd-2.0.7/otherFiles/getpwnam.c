#include <pwd.h>
#include <stdlib.h>

struct passwd *getpwnam(char const   *__name ) {
	struct passwd* x = malloc(sizeof(struct passwd));

/*    char *pw_name ; */
/*    char *pw_passwd ; */
/*    __uid_t pw_uid ; */
/*    __gid_t pw_gid ; */
/*    char *pw_gecos ; */
/*   char pw_dir[100]; */
/*    char *pw_shell ; */

	x->pw_dir = "some name goes here";
//	__uid_t uid;
	x->pw_uid = __SYMBOLIC();
//	char *p;
	x->pw_name = __SYMBOLIC();
//	__gid_t gid;
	x->pw_gid = __SYMBOLIC();

	return x;
}
