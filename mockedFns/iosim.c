#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>

#define	IOSIM_MAX_FILE	1024
int		IOSIM_num_file = 0; // Number of files currently
char* IOSIM_file_name[IOSIM_MAX_FILE]; // Array of file names
sym_file_t* IOSIM_file[IOSIM_MAX_FILE]; // Array of the files themselves

// Unless this is "/", it should not end with a '/'
char workingDir[PATH_MAX+1] = "/";

// IOSIM_fd[i] is the stream associate with descriptor i
sym_file_stream_t*	IOSIM_fd[IOSIM_MAX_FD];
// fd: 0-stdin, 1-stdout, 2-stderr
// The next available fd. Starting from 3.
int		IOSIM_num_fd = 3;

//sym_file_t* IOSIM_newbuf(int len, char* buf){
//	sym_file_stream_t* ret = malloc(sizeof(sym_file_stream_t));
//	ret->cur = 0;
//	if(len<0)
//		ret->len = strlen(buf);
//	else 
//		ret->len = len;
//	ret->buf = buf;
//	return ret;
//}

// If the file exists, return it; otherwise return NULL.
// This requires *absolute* file names.
sym_file_t *IOSIM_findfile(const char *file){
	for(int i=0;i<IOSIM_num_file;i++){
		if(strcmp(file,IOSIM_file_name[i])==0)
			return IOSIM_file[i];
	}
	return NULL;
}

// This creates the file and returns it
sym_file_t *IOSIM_addfile(const char *filename, const char *contents, mode_t mode){
	if (IOSIM_num_file >= IOSIM_MAX_FILE) {
		errno = ENFILE;
		return -1;
	}
	sym_file_t *file = malloc(sizeof(sym_file_t));
	if (contents) {
		size_t n = strlen(contents);
		file->stat.st_size = n;
		memcpy(file->contents,contents,n);
	} else {
		file->stat.st_size = 0;
		file->contents = NULL;
	}
	file->stat.st_nlink = 1; // Only one hard link

	// For now, I'm only making regular files (S_IFREG) which are readable by others (S_IROTH)
	file->stat.st_mode = (S_IFREG | mode | S_IROTH);

	// Setting (to arbitrary values) some fields that get accessed.
	// At some point, these values might want to be more 'real'.
	file->stat.st_uid = 1234;
	file->stat.st_gid = 5678;
	file->stat.st_atim.tv_sec = 1;
	file->stat.st_atim.tv_nsec = 2;
	file->stat.st_mtim.tv_sec = 1;
	file->stat.st_mtim.tv_nsec = 2;
	file->stat.st_ctim.tv_sec = 1;
	file->stat.st_ctim.tv_nsec = 2;

	IOSIM_file_name[IOSIM_num_file] = strdup(filename);
	IOSIM_file[IOSIM_num_file] = file;
	IOSIM_num_file++;

	return file;
}

int IOSIM_newfd(){
	if (IOSIM_num_fd >= IOSIM_MAX_FD) {
		__COMMENT("Too many file descriptors");
		exit(1);
	}
	int ret = IOSIM_num_fd;
	IOSIM_num_fd ++;
	return ret;
}

// Allocates and returns a string which holds the absolute path for 'name'
char *IOSIM_toAbsolute(const char *name) {
	char *absoluteName = malloc(PATH_MAX+1);
	char *lastSlash = strrchr(name,'/');
	if (lastSlash) { // If 'name' is not a bare file name
		if (lastSlash == name) { // If name is "/<something without a slash>"
			strcpy(absoluteName,"/");
		} else {
			char *path = strdup(name);
			path[lastSlash - name] = 0; // Make 'path' be just the path, excluding the file name
			if (!realpath(path,absoluteName)) { // Make the path absolute
				free(path);
				return NULL; // Return NULL on error
			}
			free(path);
		}
	} else { // 'name' contained no path at all, only a file name
		strcpy(absoluteName,workingDir);
	}
	// At this point, absoluteName is the absolute path, but only the path
	char *end = strchr(absoluteName,0);
	// Potential buffer overflow here if (absolute path + file name) is too long
	if (absoluteName[1]) { // If absoluteName is not "/", so it doesn't end with a '/'
		*end++ = '/'; // add a '/'
	}
	strcpy(end, lastSlash ? lastSlash + 1 : name); // Add the file name
	return absoluteName;
}

static mode_t usermask;
mode_t umask(mode_t cmask) {
	mode_t oldMask = usermask;
	usermask = cmask & 0777;
	return oldMask;
}

int IOSIM_openWithMode(const char *name, int flags, mode_t mode) {
	char *absoluteName = IOSIM_toAbsolute(name);
	sym_file_t *sym_file = IOSIM_findfile(absoluteName);
	if (sym_file) {
		free(absoluteName);
		// If the file exists, then return an error if it shouldn't
		if ((flags & O_CREAT) && (flags & O_EXCL)) {
			errno = EEXIST;
			return -1;
		}
	} else if (flags & O_CREAT) {
		// File doesn't exist and we should create it
		sym_file = IOSIM_addfile(absoluteName, NULL, mode & ~usermask);
		free(absoluteName);
	} else {
		// File doesn't exist, and we shouldn't create it.
		errno = ENOENT;
		free(absoluteName);
		return -1;
	}

	// Create a new stream for the file
	sym_file_stream_t *sym_stream = malloc(sizeof(sym_file_stream_t));

	// Put the stream into the 'descriptor table'.
	int fd = IOSIM_newfd();
	IOSIM_fd[fd] = sym_stream;

	// Initialize the values for the stream
	sym_stream->sym_file = sym_file;
	sym_stream->offset = 0;
	sym_stream->fd = fd;

	return fd;
}

int IOSIM_open(const char *name, int flags) {
	return IOSIM_openWithMode(name,flags,0777);
}

sym_file_stream_t *IOSIM_getStream(int fd) {
	return IOSIM_fd[fd];
}

int IOSIM_rename(const char *old, const char *new) {
	// Compute the absolute path names, returning -1 if there is a problem
	char *absOld = IOSIM_toAbsolute(old);
	if (!absOld) {
		free(absOld);
		return -1;
	}
	char *absNew = IOSIM_toAbsolute(new);
	if (!absNew) {
		free(absOld);
		free(absNew);
		return -1;
	}
	for(int i=0;i<IOSIM_num_file;i++){
		if(strcmp(absOld,IOSIM_file_name[i])==0) {
			// We found the old name.
			if (IOSIM_findfile(absNew)) {
				// A file with the new name already exists. Overwrite it.
				IOSIM_unlink(absNew);
			}
			free(IOSIM_file_name[i]);
			free(absOld);
			// Don't free absNew in this case
			IOSIM_file_name[i] = absNew;
			return 0;
		}
	}
	// The file "old" doesn't exist
	free(absOld);
	free(absNew);
	errno = ENOENT;
	return -1;
}
int IOSIM_unlink(const char *pathname) {
	char *absoluteName = IOSIM_toAbsolute(pathname);
	for (int i = 0; i < IOSIM_num_file; i++) {
		if (strcmp(absoluteName, IOSIM_file_name[i]) == 0) {
			free(absoluteName);
			free(IOSIM_file_name[i]);
			sym_file_t *sym_file = IOSIM_file[i];
			// Decrement the hard-link count
			sym_file->stat.st_nlink--;
			// If the count goes to 0, delete the file
			if (sym_file->stat.st_nlink == 0) {
				free(sym_file->contents);
				free(sym_file);
			}
			// Remove the file entry from IOSIM_file and IOSIM_file_name
			// This may mess up readdir(), though, because it shifts indices.
			IOSIM_num_file--;
			for ( ; i < IOSIM_num_file; i++) {
				IOSIM_file[i] = IOSIM_file[i+1];
				IOSIM_file_name[i] = IOSIM_file_name[i+1];
			}
			// We unlinked successfully
			return 0;
		}
	}
	free(absoluteName);
	errno = ENOENT;
	return -1;
}

int IOSIM_close(int fildes) {
	if (fildes < 0 || fildes >= IOSIM_num_fd || IOSIM_fd[fildes] == NULL) {
		errno = EBADF;
		return -1;
	}
	IOSIM_fd[fildes] = NULL;
	return 0;
}

int IOSIM_read(int fildes, void *buf, size_t nbyte){
	size_t n;
	int cur,len;
	char* cbuf = buf;

	if(nbyte == 0) return 0;

	//	if(fildes == 0) return -1;

	sym_file_stream_t* in = IOSIM_fd[fildes];

	cur = in->offset;
	len = in->sym_file->stat.st_size;
	if (in->buffer == NULL){
		in->buffer=memcpy(malloc(len),in->sym_file->contents,len);
	}
	for(n=0;n<nbyte;n++){
		if(cur>=len) {
			cbuf[n] = EOF;
			break;
		}
		cbuf[n] = in->buffer[cur];

		cur++;
	}
	in->offset = cur;
	return n;
}

int IOSIM_ungetc(int c, int fildes){

	sym_file_stream_t* in = IOSIM_fd[fildes];

	in->offset--;
	in->buffer[in->offset] = (char)c;

	return c;
}

int IOSIM_write(int fildes, const void *buf, size_t nbyte){
	int cur,len;
	char* cbuf = buf;

	if(nbyte == 0) return 0;
	if(fildes == 0) {
		__COMMENT("Writing on fildes 0");
		__EVALSTR(buf,nbyte);
		return -1;
	}

	sym_file_stream_t* out = IOSIM_fd[fildes];
	cur = out->offset;
	len = out->sym_file->stat.st_size;

	// Enlarge file if necessary
	if (cur + nbyte > len) {
		out->sym_file->contents = realloc(out->sym_file->contents, cur + nbyte);
		out->sym_file->stat.st_size = cur + nbyte;
	}

	memcpy(out->sym_file->contents + cur, cbuf, nbyte);

	__COMMENT("Writing on fildes");
	__EVAL(fildes);
	__EVALSTR(buf,nbyte);
	__COMMENT("Done");

	// Update offset pointer
	out->offset += nbyte;

	return nbyte;
}

int IOSIM_eof(int fildes){
        int cur,len;

	sym_file_stream_t* in = IOSIM_fd[fildes];

        cur = in->offset;
        len = in->sym_file->stat.st_size;

        return cur >= len;
}

// This doesn't check that the destination directory exists		
int IOSIM_chdir(const char *path) {
	// Work with a temporary string so workingDir only changes if
	// realpath() succeeds.
	char newWorkingDir[PATH_MAX+1];
	if (realpath(path,newWorkingDir)) {
		strcpy(workingDir,newWorkingDir);
		return 0;
	}
	return -1;
}

//int chroot(const char *path) {
//	return 0;
//}

// For now, a struct __dirstream (aka, a DIR) contains:
//  1. The name of the directory as an absolute path
//  2. An index into the array of all file names
//  3. A struct dirent which we return when readdir is called on this DIR
//  4. A sym_file_stream_t, so we can treat the directory sort of like a file
typedef struct __dirstream {
	char dirname[PATH_MAX+1];
	int index;
	struct dirent readdirRetval;
	sym_file_stream_t filestream;
};

void initDir(DIR *dir, int i) {
	dir->index = i;
	dir->filestream.fd = IOSIM_newfd();
	dir->filestream.sym_file = malloc(sizeof(sym_file_t));
	dir->filestream.sym_file->stat.st_mode = S_IROTH;
	IOSIM_fd[dir->filestream.fd] = &dir->filestream;
}

DIR *IOSIM_opendir(const char *dirname) {
	DIR *dir = malloc(sizeof(DIR));
	if (!realpath(dirname,dir->dirname)) {
		free(dir);
		return NULL;
	}
	int length = strlen(dir->dirname);
	if (length == 1) { // dir->dirname is "/", so it exists
		initDir(dir,0);
		return dir;
	}
	for (int i = 0; i < IOSIM_num_file; i++) {
		char *filename = IOSIM_file_name[i];
		// In this next 'if', the strncmp() checks that filename is a
		// prefix of dir->dirname and the expression to the right of the
		// '&&' makes sure what we find is really a directory.
		// Since IOSIM_file_name only contains file names (and not
		// directory names), if filename really is a file in dir->dirname
		// (or a subdirectory), the next character will be '/'. Otherwise,
		// filename is a prefix but *not* the directory we're looking for.
		// (e.g., "/foo" should match "/foo/bar", but not "/foot" or "/foot/bar")
		if (!strncmp(dir->dirname,filename,length) && filename[length] == '/') {
			// filename is the name of a file in directory dirname (or in a subdirectory),
			// so the directory exists and i is the first index of a file in the directory
			initDir(dir,i);
			return dir;
		}
	}
	// The directory does not exist
	errno = ENOENT;
	free(dir);
	return NULL;
}

int IOSIM_closedir(DIR *dir) {
	IOSIM_fd[dir->filestream.fd] = NULL;
	free(dir);
	return 0;
}

struct dirent *IOSIM_readdir(DIR *dir) {
	while (dir->index < IOSIM_num_file) {
		// See if the file name is "<dirname>/<something without slashes>"
		// (or just "/<something without slashes>" if <dirname> is "/")
		char *filename = IOSIM_file_name[dir->index];
		dir->index++;
		char *lastSlash = strrchr(filename,'/');
		int dirContainsThisFile;
		if (lastSlash == filename) { // The file is in '/'
			dirContainsThisFile = !strcmp("/",dir->dirname);
		} else {
			*lastSlash = 0; // Overwrite the final '/' with null, leaving only filename's directory
			dirContainsThisFile = !strcmp(filename,dir->dirname); // Check that the path is dir->dirname
			*lastSlash = '/'; // Restore the full file name
		}
		if (dirContainsThisFile) {
			if (strlen(filename) >= sizeof(dir->readdirRetval.d_name)) {
				errno = EOVERFLOW;
				return NULL;
			}
			strcpy(dir->readdirRetval.d_name,lastSlash+1);
			return &dir->readdirRetval;
		}
	}
	return NULL;
}

char *IOSIM_getcwd(char *buf, size_t size) {
	if (size == 0) { // Special case for size == 0
		if (buf) {
			errno = EINVAL;
			return NULL;
		}
		buf = malloc(strlen(workingDir) + 1);
		return strcpy(buf,workingDir);
	}
	if (size <= strlen(workingDir)) { // If size is too small
		errno = ERANGE;
		return NULL;
	}
	if (!buf) { // We know size > strlen(workingDir), so if buf is NULL, call malloc
		buf = malloc(size);
	}
	return strcpy(buf,workingDir);
}

int IOSIM_dirfd(DIR *dir) {
	return dir->filestream.fd;
}

int IOSIM_socket(int domain, int type, int protocol) {
	// I guess we need a socket table, like we have a file table
	sym_file_stream_t *stream = malloc(sizeof(sym_file_stream_t));
	stream->offset = 0;
	stream->sym_file = malloc(sizeof(sym_file_t));
	stream->sym_file->contents = NULL;
	stream->sym_file->stat.st_mode = S_IFSOCK;
	int fd = IOSIM_newfd();
	stream->fd = fd;
	IOSIM_fd[fd] = stream;
	return fd;
}