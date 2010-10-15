#ifndef _SYS_STAT_H
#define _SYS_STAT_H

#include <sys/types.h>

struct stat
{
	dev_t st_dev;
	ino_t st_ino;
	mode_t st_mode;
	nlink_t st_nlink;
	uid_t st_uid;
	gid_t st_gid;
	dev_t st_rdev;
	off_t st_size;
	time_t st_atime;
	time_t st_mtime;
	time_t st_ctime;
	blksize_t st_blksize;
	blkcnt_t st_blocks;
};

/* file modes types */
#define S_IFBLK 0x00000000
#define S_IFCHR 0x00010000
#define S_IFIFO 0x00020000
#define S_IFREG 0x00040000
#define S_IFDIR 0x00080000
#define S_IFLNK 0x00100000
#define S_IFSMQ 0x00200000
#define S_IFSEM 0x00400000
#define S_IFSHM 0x00800000
#define S_IFSOCK 0x01000000

#define S_ISBLK(s) (s & S_IFBLK)
#define S_ISCHR(s) (s & S_IFCHR)
#define S_ISFIFO(s) (s & S_IFIFO)
#define S_ISREG(s) (s & S_IFREG)
#define S_ISDIR(s) (s & S_IFDIR)
#define S_ISLNK(s) (s & S_IFLNK)
#define S_ISSMQ(s) (s & S_IFSMQ)
#define S_ISSEM(s) (s & S_IFSEM)
#define S_ISSHM(s) (s & S_IFSHM)
#define S_ISSOCK(s) (s & S_IFSOCK)

#define S_TYPEISMQ(s) S_ISSMQ(s)
#define S_TYPEISEM(s) S_ISSEM(s)
#define S_TYPEISHM(s) S_ISSHM(s)

/* permission bits */

#define S_ISUSR 0x2000 /* 0010 0000 0000 0000 */
#define S_ISGRP 0x1000 /* 0001 0000 0000 0000 */

#define S_ISUID 0x0800 /* 0000 1000 0000 0000 */
#define S_ISGID 0x0400 /* 0000 0100 0000 0000 */
#define S_ISVTX 0x0200 /* 0000 0010 0000 0000 */

#define S_IRWXU 0x01C0 /* 0000 0001 1100 0000 */
#define S_IRUSR 0x0100 /* 0000 0001 0000 0000 */
#define S_IWUSR 0x0080 /* 0000 0000 1000 0000 */
#define S_IXUSR 0x0040 /* 0000 0000 0100 0000 */

#define S_IRWXG 0x0038 /* 0000 0000 0011 1000 */
#define S_IRGRP 0x0020 /* 0000 0000 0010 0000 */
#define S_IWGRP 0x0010 /* 0000 0000 0001 0000 */
#define S_IXGRP 0x0008 /* 0000 0000 0000 1000 */

#define S_IRWXO 0x0007 /* 0000 0000 0000 0111 */
#define S_IROTH 0x0004 /* 0000 0000 0000 0100 */
#define S_IWOTH 0x0002 /* 0000 0000 0000 0010 */
#define S_IXOTH 0x0001 /* 0000 0000 0000 0001 */

int chmod(const char* name, mode_t mode);
int fchmod(int file, mode_t mode);
int fstat(int file, struct stat* s);
int lstat(const char* name, struct stat* s);
int mkdir(const char* name, mode_t mode);
int mkfifo(const char* name, mode_t mode);
int mknod(const char* name, mode_t mode, dev_t);
int stat(const char* name, struct stat* s);
mode_t umask(mode_t mode);


#endif
