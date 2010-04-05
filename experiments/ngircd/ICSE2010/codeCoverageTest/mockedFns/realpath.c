// From uClibc

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

char *realpath(const char *path, char got_path[])
{
	char copy_path[PATH_MAX];
	/* use user supplied buffer directly - reduces stack usage */
	/* char got_path[PATH_MAX]; */
	char *max_path;
	char *new_path;
	size_t path_len;
	int readlinks = 0;
#ifdef S_IFLNK
	int link_len;
#endif

	if (path == NULL) {
		errno = EINVAL;
		return NULL;
	}
	if (*path == '\0') {
		errno = ENOENT;
		return NULL;
	}
	/* Make a copy of the source path since we may need to modify it. */
	path_len = strlen(path);
	if (path_len >= PATH_MAX - 2) {
		errno = ENAMETOOLONG;
		return NULL;
	}
	/* Copy so that path is at the end of copy_path[] */
	strcpy(copy_path + (PATH_MAX-1) - path_len, path);
	path = copy_path + (PATH_MAX-1) - path_len;
	max_path = got_path + PATH_MAX - 2; /* points to last non-NUL char */
	new_path = got_path;
	if (*path != '/') {
		/* If it's a relative pathname use getcwd for starters. */
		if (!getcwd(new_path, PATH_MAX - 1))
			return NULL;
		new_path += strlen(new_path);
		if (new_path[-1] != '/')
			*new_path++ = '/';
	} else {
		*new_path++ = '/';
		path++;
	}
	/* Expand each slash-separated pathname component. */
	while (*path != '\0') {
		/* Ignore stray "/". */
		if (*path == '/') {
			path++;
			continue;
		}
		if (*path == '.') {
			/* Ignore ".". */
			if (path[1] == '\0' || path[1] == '/') {
				path++;
				continue;
			}
			if (path[1] == '.') {
				if (path[2] == '\0' || path[2] == '/') {
					path += 2;
					/* Ignore ".." at root. */
					if (new_path == got_path + 1)
						continue;
					/* Handle ".." by backing up. */
					while ((--new_path)[-1] != '/');
					continue;
				}
			}
		}
		/* Safely copy the next pathname component. */
		while (*path != '\0' && *path != '/') {
			if (new_path > max_path) {
				errno = ENAMETOOLONG;
				return NULL;
			}
			*new_path++ = *path++;
		}
#ifdef S_IFLNK
		/* Protect against infinite loops. */
		if (readlinks++ > MAX_READLINKS) {
			errno = ELOOP;
			return NULL;
		}
		path_len = strlen(path);
		/* See if last (so far) pathname component is a symlink. */
		*new_path = '\0';
		{
			int sv_errno = errno;
			link_len = readlink(got_path, copy_path, PATH_MAX - 1);
			if (link_len < 0) {
				/* EINVAL means the file exists but isn't a symlink. */
				if (errno != EINVAL) {
					return NULL;
				}
			} else {
				/* Safe sex check. */
				if (path_len + link_len >= PATH_MAX - 2) {
					errno = ENAMETOOLONG;
					return NULL;
				}
				/* Note: readlink doesn't add the null byte. */
				/* copy_path[link_len] = '\0'; - we don't need it too */
				if (*copy_path == '/')
					/* Start over for an absolute symlink. */
					new_path = got_path;
				else
					/* Otherwise back up over this component. */
					while (*(--new_path) != '/');
				/* Prepend symlink contents to path. */
				memmove(copy_path + (PATH_MAX-1) - link_len - path_len, copy_path, link_len);
				path = copy_path + (PATH_MAX-1) - link_len - path_len;
			}
			errno = sv_errno;
		}
#endif							/* S_IFLNK */
		*new_path++ = '/';
	}
	/* Delete trailing slash but don't whomp a lone slash. */
	if (new_path != got_path + 1 && new_path[-1] == '/')
		new_path--;
	/* Make sure it's null terminated. */
	*new_path = '\0';
	return got_path;
}
