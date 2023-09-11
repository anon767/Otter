#ifndef _PWD_H
#define _PWD_H

#include<sys/types.h>

struct passwd
{
	char* pw_name;
	uid_t pw_uid;
	gid_t pw_gid;
	char* pw_dir;
	char* pw_shell;
	char* pw_passwd;
};

struct passwd *getpwnam(const char *name);
struct passwd *getpwuid(uid_t uid);
int getpwnam_r(const char *name, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result);
int getpwuid_r(uid_t uid, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result);
void endpwent();
struct passwd *getpwent();
void setpwent();


#endif
