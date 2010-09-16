#ifndef _DIREND_H
#define _DIRENT_H

#include<sys/types.h>

typedef struct
{
	ino_t d_ino;
	char d_name[];
} DIR;

/*int closedir(DIR* dirp);
DIR* opendir(const char* name);
struct dirent* readdir(DIR* dirp);
int readdir_r(DIR* dirp, struct dirent* entry, struct dirent **result);
void rewinddir(DIR* dirp);
void seekdir(DIR* dirp, long int loc);
long int telldir(DIR* dirp);*/


#endif
