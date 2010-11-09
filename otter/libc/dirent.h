#ifndef _DIREND_H
#define _DIRENT_H

#include <sys/types.h>
#include <__otter/otter_fs.h>

struct dirent
{
	ino_t d_ino;
	char d_name[];
};

typedef struct
{
	struct __otter_fs_dnode* dnode;
} DIR;

/*int closedir(DIR* dirp);
DIR* opendir(const char* name);
struct dirent* readdir(DIR* dirp);
int readdir_r(DIR* dirp, struct dirent* entry, struct dirent **result);
void rewinddir(DIR* dirp);
void seekdir(DIR* dirp, long int loc);
long int telldir(DIR* dirp);*/


#endif
