#include <grp.h>
#include <__otter/otter_user.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

struct group *__otter_libc_getgrgid(gid_t gid)
{
	struct group* grp = malloc(sizeof(struct group));
	char* buf = malloc((sizeof(char*) * 2) + 5);
	struct group* r = NULL;
	
	if(getgrgid_r(gid, grp, buf, (sizeof(char*) * 2) + 5, &r) == 0)
		return(grp);
	
	free(grp);
	free(buf);
	return(NULL);
}

int __otter_libc_getgrgid_r(gid_t gid, struct group *grp, char *buffer, size_t bufsize, struct group **result)
{
	switch(gid)
	{
		case __otter_GID_ROOT:
		{
			char** cbuf = (char**)buffer;
			char* sbuf = (char*)(cbuf + 2);
			
			grp->gr_name = "root";
			grp->gr_gid = __otter_GID_ROOT;
			grp->gr_mem = cbuf;
			
			char** cbuf = (char**)buffer;
			char* sbuf = (char*)(cbuf + 2);
			
			if(bufsize >= (sizeof(char*) * 2) + 5)
			{
				cbuf[0] = sbuf;
				cbuf[1] = NULL;
				sbuf[0] = 'r';
				sbuf[1] = 'o';
				sbuf[2] = 'o';
				sbuf[3] = 't';
				sbuf[4] = 0;
			}
			else
			{
				*result = NULL;
				return(ERANGE);
			}
			
			*result = grp;
			return(0);
		}
		case __otter_GID_USER:
		{
			char** cbuf = (char**)buffer;
			char* sbuf = (char*)(cbuf + 2);
			
			grp->gr_name = "user";
			grp->gr_gid = __otter_GID_USER;
			grp->gr_mem = buffer;
			
			if(bufsize >= (sizeof(char*) * 2) + 5)
			{
				cbuf[0] = sbuf;
				cbuf[1] = NULL;
				sbuf[0] = 'u';
				sbuf[1] = 's';
				sbuf[2] = 'e';
				sbuf[3] = 'r';
				sbuf[4] = 0;
			}
			else
			{
				*result = NULL;
				return(ERANGE);
			}
			
			*result = grp;
			return(0);
		}
		default:
			*result = NULL;
			return(0);
	}
}

struct group *__otter_libc_getgrnam(const char *name)
{
	if(strcmp("root", name) == 0)
	{
		return getgrgid(__otter_GID_ROOT);
	}
	else if(strcmp("user", name) == 0)
	{
		return getgrgid(__otter_GID_USER);
	}
	
	return NULL;
}

int __otter_libc_getgrnam_r(const char *name, struct group *grp, char *buffer, size_t bufsize, struct group **result)
{
	if(strcmp("root", name) == 0)
	{
		return getgrgid_r(__otter_GID_ROOT, grp, buffer, bufsize, result);
	}
	else if(strcmp("user", name) == 0)
	{
		return getgrgid_r(__otter_GID_USER, grp, buffer, bufsize, result);
	}
	
	return getgrgid_r(__otter_GID_INVALID, grp, buffer, bufsize, result);
}

gid_t __otter_libc_getgrent_gid = __otter_GID_ROOT;
struct group *getgrent()
{
	gid_t gid = __otter_libc_getgrent_gid;
	switch(__otter_libc_getgrent_gid)
	{
		case __otter_GID_ROOT:
			__otter_libc_getgrent_gid = __otter_GID_USER;
		case __otter_GID_USER:
			__otter_libc_getgrent_gid = __otter_GID_INVALID;
		default:
			return NULL;
	}
	
	return __otter_libc_getgrgid(gid);
}

void __otter_libc_endgrent()
{
	/* do nothing */
}

void __otter_libc_setgrent()
{
	/* reset to first entry */
	__otter_libc_getgrent_gid = __otter_GID_ROOT;
}

int setgroups(size_t n, const gid_t* groups)
{
	/* suplimental groups are not implimented */
	errno = EINVAL;
	return(-1);
}

int initgroups(const char* user, gid_t group)
{
	/* suplimental groups are not implimented; this does nothing */
	return(0);
}

