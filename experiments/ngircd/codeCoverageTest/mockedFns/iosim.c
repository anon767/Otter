#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <dirent.h>
#include "umask.c"
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


char *realpath(const char *restrict file_name,
	       char *restrict resolved_name){
	strcpy(resolved_name,file_name);
	strcat(resolved_name,"/");
	return resolved_name;
}


// If the file exists, return it; otherwise return NULL.
sym_file_t *IOSIM_findfile(const char *file){
	for(int i=0;i<IOSIM_num_file;i++){
		if(strcmp(file,IOSIM_file_name[i])==0)
			return IOSIM_file[i];
	}
	return NULL;
}

// This creates the file and returns it
sym_file_t *IOSIM_addfile(const char *filename, mode_t mode){
	if (IOSIM_num_file >= IOSIM_MAX_FILE) {
		errno = EMFILE;
		return -1;
	}
	if (IOSIM_num_fd >= IOSIM_MAX_FD) {
		errno = ENFILE;
		return -1;
	}
	sym_file_t *file = malloc(sizeof(sym_file_t));
	file->stat.st_size = 0;
	file->contents = NULL;
	file->stat.st_mode = (S_IFREG | mode | S_IROTH); // For now, I'm only making regular files (S_IFREG) which are readable by others (S_IROTH)
	file->stat.st_nlink = 1; // Only one hard link
	file->stat.st_uid = 1234; // Does this need to be a real value?
	file->stat.st_gid = 5678; // Does this need to be a real value?
	file->stat.st_atim.tv_sec = 1;
	file->stat.st_atim.tv_nsec = 2;
	file->stat.st_mtim.tv_sec = 1;
	file->stat.st_mtim.tv_nsec = 2;
	file->stat.st_ctim.tv_sec = 1;
	file->stat.st_ctim.tv_nsec = 2;
	// I probably have to set some more fields here.

	__COMMENT("Adding file");
	__EVALSTR(filename,strlen(filename));

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

int IOSIM_openWithMode(const char *name, int flags, mode_t mode) {
	char absoluteName[PATH_MAX+1];
	char *lastSlash = strrchr(name,'/');
	if (lastSlash) { // If 'name' is not a bare file name
		char *path = strdup(name);
		path[lastSlash - name] = 0; // Make 'path' be just the path, excluding the file name
		if (!realpath(path,absoluteName)) { // Make the path absolute
			return -1; // Return -1 on error
		}
		free(path);
	} else { // There was no path at all, only a file name
		strcpy(absoluteName,workingDir);
	}
	// At this point, absoluteName is the absolute path, but only the path
	char *end = strchr(absoluteName,0);
	if (absoluteName[1]) { // If absoluteName is not "/", so it doesn't end with a '/'
		*end++ = '/'; // add a '/'
	}
	strcpy(end, lastSlash ? lastSlash + 1 : name); // Add the file name
	sym_file_t *sym_file = IOSIM_findfile(absoluteName);
	if (sym_file) {
		// If the file exists, then return an error if it shouldn't
		if ((flags & O_CREAT) && (flags & O_EXCL)) {
			errno = EEXIST;
			return -1;
		}
	} else if (flags & O_CREAT) {
		// File doesn't exist and we should create it
		sym_file = IOSIM_addfile(absoluteName, mode & ~usermask);
	} else {
		// File doesn't exist, and we shouldn't create it.
		errno = ENOENT;
		return -1;
	}

	// Create a new stream for the file
	sym_file_stream_t *sym_stream = malloc(sizeof(sym_file_stream_t));

	// Put the stream into the 'descriptor table'.
	int fd = IOSIM_newfd();
	IOSIM_fd[fd] = sym_stream;

	// Initialize the values for the stream
	sym_stream->sym_file = sym_file;
	sym_stream->sym_fileout = sym_file;
	sym_stream->offset = 0;
	sym_stream->fd = fd;

	return fd;
}

int IOSIM_open(const char *name, int flags) {
	return IOSIM_openWithMode(name,flags,0777);
}

int IOSIM_close(int fildes) {
	if (fildes < 0 || fildes >= IOSIM_num_fd || IOSIM_fd[fildes] == NULL) {
		errno = EBADF;
		return -1;
	}
	IOSIM_fd[fildes] = NULL;
	return 0;
}

int IOSIM_read(int fildes, void *buf, int nbyte){
	int n,cur,len;
	char* cbuf = buf;

	if(nbyte == 0) return 0;
	//	if(fildes == 0) return -1;

	sym_file_stream_t* in = IOSIM_fd[fildes];
	cur = in->offset;
	if (in->buffer == NULL){
		in->buffer=strdup(in->sym_file->contents);
	}
	len = in->sym_file->stat.st_size;
	for(n=0;n<nbyte;n++){
		if(cur>=len) {
			cbuf[n] = EOF;
			break;
		}
		cbuf[n] = in->buffer[cur];

		cur++;
	}
	in->offset = cur;
	if (n < 0) {
		__COMMENT("n is negative in IOSIM_read; this shouldn't be");
		exit(1);
	}
	return n;
}

int IOSIM_ungetc(int c, int fildes){
	sym_file_stream_t* in = IOSIM_fd[fildes];

	in->offset--;
	in->buffer[in->offset] = (char)c;

	return c;
}

int IOSIM_write(int fildes, const void *buf, int nbyte){

	int n,cur,len;
	char* cbuf = buf;

	static int numWritten[IOSIM_MAX_FD]; // Initialized to 0

	if(nbyte == 0) return 0;
	if(fildes == 0) return -1;

	sym_file_stream_t* out = IOSIM_fd[fildes];
	// tricky: redirect to the real writing stream
	//if(out->fd_type==IOSIM_FD_SOCK){
	//	return IOSIM_write(fildes+1,buf,nbyte);
	//}
	cur = out->offsetout;
	len = out->sym_fileout->stat.st_size;

	// Enlarge file if necessary
	if (cur + nbyte > len) {
		out->sym_fileout->contents = realloc(out->sym_fileout->contents, cur + nbyte);
		out->sym_fileout->stat.st_size = cur + nbyte;
		len = cur + nbyte;
	}

	memcpy(out->sym_fileout->contents + cur, cbuf, nbyte);
//	for(n=0;n<nbyte;n++){
//		if(cur>=len) break; // I don't think this ever happens
//		out->sym_fileout->contents[cur] = cbuf[n];
//		cur++;
//	}

	/* Print out, for the benefit of the person running the symbolic
		 executor, everything written on this file so far. */
	numWritten[fildes] += nbyte;
	__EVALSTR(out->sym_fileout->contents,numWritten[fildes]);

	// Update offsetout pointer
	out->offsetout += nbyte;

	return nbyte;
}

int IOSIM_eof(int fildes){
        int cur,len;

        sym_file_stream_t* in = IOSIM_fd[fildes];
        cur = in->offset;
        len = in->sym_file->stat.st_size;

        return cur >= len;
}
		
int IOSIM_chdir(const char *path) {
	if (realpath(path,workingDir)) {
		return 0;
	}
	return -1;
}

//int chroot(const char *path) {
//	return 0;
//}

// For now, a struct __dirstream (aka, a DIR) contains:
//  1. The name of the directory as an absolute path and ending with a slash
//  2. An index into the array of all file names
//  3. A struct dirent which we return when readdir is called on this DIR
//  4. A sym_file_stream_t, so we can treat the directory sort of like a file
typedef struct __dirstream {
	char dirname[PATH_MAX+1];
	int index;
	struct dirent readdirRetval;
	sym_file_stream_t filestream;
};

DIR *IOSIM_opendir(const char *dirname) {
	DIR *dir = malloc(sizeof(DIR));
	if (!realpath(dirname,dir->dirname)) {
		return NULL;
	}
	dir->index = 0;
	dir->filestream.fd = IOSIM_newfd();
	dir->filestream.sym_file = malloc(sizeof(sym_file_t));
	dir->filestream.sym_file->stat.st_mode = S_IROTH;
	IOSIM_fd[dir->filestream.fd] = &dir->filestream;
	return dir;
}

int IOSIM_closedir(DIR *dir) {
	IOSIM_fd[dir->filestream.fd] = NULL;
	free(dir);
	return 0;
}

int dirContainsFile(const char *dirname, const char *filename) {
	// Check that filename is "<dirname>/<something without slashes>"
	char *lastSlash = strrchr(filename,'/');
	return !strncmp(filename,dirname,lastSlash - filename);
}

/* Right now, this returns the full, absolute path of the next file in
	 the directory. Is this the right thing to do? */
struct dirent *IOSIM_readdir(DIR *dir) {
	while (dir->index < IOSIM_num_file) {
		// See if the file is "<dirname>/<something without slashes>"
		char *filename = strdup(IOSIM_file_name[dir->index]);
		dir->index++;
		char *lastSlash = strrchr(filename,'/');
		*lastSlash = 0; // Overwrite the final '/' with null
		if (!strcmp(filename,dir->dirname)) { // Check that the path is dir->dirname
			*lastSlash = '/'; // Restore the full file name
			if (strlen(filename) >= sizeof(dir->readdirRetval.d_name)) {
				errno = EOVERFLOW;
				free(filename);
				return NULL;
			}
			strcpy(dir->readdirRetval.d_name,filename);
			free(filename);
			return &dir->readdirRetval;
		}
		free(filename);
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
	if (!buf) { // We know size > 0, so if buf is NULL, call malloc
		buf = malloc(size);
	}
	return strcpy(buf,workingDir);
}

int IOSIM_dirfd(DIR *dir) {
	return dir->filestream.fd;
}


void IOSIM_updatesize(int fildes, int t){
	if(t>=IOSIM_MAX_EVENTS){
		__COMMENT("Error: too many events!");
		exit(1);
	}
	sym_file_stream_t* f = IOSIM_fd[fildes];
	f->sym_file->stat.st_size = f->sym_file->size[t];
}



