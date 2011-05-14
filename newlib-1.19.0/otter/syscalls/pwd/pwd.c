#include <pwd.h>
#include <otter/otter_user.h>
#include <stdlib.h>
#include <string.h>

struct passwd *getpwnam(const char *name)
{
	if(strcmp("root", name) == 0)
	{
		return getpwuid(__otter_UID_ROOT);
	}
	else if(strcmp("user", name) == 0)
	{
		return getpwuid(__otter_UID_USER);
	}
	
	return NULL;
}

struct passwd *getpwuid(uid_t uid)
{
	struct passwd* pwd = malloc(sizeof(struct passwd));
	char* buf = malloc((sizeof(char*) * 2) + 5);
	struct passwd* r = NULL;
	
	if(getpwuid_r(uid, pwd, buf, (sizeof(char*) * 2) + 5, &r) == 0)
		return(pwd);
	
	free(pwd);
	free(buf);
	return(NULL);
}

int getpwnam_r(const char *name, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result)
{
	if(strcmp("root", name) == 0)
	{
		return getpwuid_r(__otter_UID_ROOT, pwd, buffer, bufsize, result);
	}
	else if(strcmp("user", name) == 0)
	{
		return getpwuid_r(__otter_UID_USER, pwd, buffer, bufsize, result);
	}
	
	return getpwuid_r(__otter_UID_INVALID, pwd, buffer, bufsize, result);
}

int getpwuid_r(uid_t uid, struct passwd *pwd, char *buffer, size_t bufsize, struct passwd **result)
{
	switch(uid)
	{
		case __otter_UID_ROOT:
			pwd->pw_name = "root";
			pwd->pw_gid = __otter_GID_ROOT;
			pwd->pw_uid = __otter_UID_ROOT;
			pwd->pw_dir = "/wrk";
			pwd->pw_shell = "";
			pwd->pw_passwd = "";
			pwd->pw_gecos = "";
			
			*result = pwd;
			return(0);
		case __otter_UID_USER:
			pwd->pw_name = "user";
			pwd->pw_gid = __otter_GID_USER;
			pwd->pw_uid = __otter_UID_USER;
			pwd->pw_dir = "/wrk";
			pwd->pw_shell = "";
			pwd->pw_passwd = "";
			pwd->pw_gecos = "";
			
			*result = pwd;
			return(0);
		default:
			*result = NULL;
			return(0);
	}
}

uid_t __otter_libc_getpwent_uid = __otter_UID_ROOT;
void endpwent()
{
	/* do nothing */
}

struct passwd *getpwent()
{
    if (__otter_libc_getpwent_uid == __otter_UID_INVALID)
        return NULL;
    else
        return getpwuid(__otter_libc_getpwent_uid);
}

void setpwent()
{
	/* reset to first entry */
	__otter_libc_getpwent_uid = __otter_UID_ROOT;
}
