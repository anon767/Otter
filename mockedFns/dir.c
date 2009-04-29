#include "iosim.h"
#include <dirent.h>

int chdir(const char *path) {
	return IOSIM_chdir(path);
}

int chroot(const char *path) {
	return 0;
}

DIR *opendir(const char *dirname) {
	return IOSIM_opendir(dirname);
}

int closedir(DIR *dir) {
	return IOSIM_closedir(dir);
}

int dirfd(DIR *dir) {
	return IOSIM_dirfd(dir);
}

struct dirent *readdir(DIR *dir) {
	return IOSIM_readdir(dir);
}

int mkdir(const char *pathname, mode_t mode) {
	return 0;
}

int rmdir(const char *pathname) {
	return 0;
}
