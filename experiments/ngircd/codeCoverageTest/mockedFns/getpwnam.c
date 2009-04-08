#include <pwd.h>
#include <stdlib.h>

struct passwd *getpwnam(char const   *__name ) {
	// assert( strcmp(__name,"nobody")==0 );
	struct passwd* x = malloc(sizeof(struct passwd));
	
	x->pw_uid = 1;
	x->pw_gid = 1;

	return x;
}
