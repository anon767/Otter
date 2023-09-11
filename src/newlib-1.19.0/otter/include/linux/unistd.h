/* TODO: fill in tons of macros indicating unsupported features */
#ifndef _LINUX_UNISTD_H
#define _LINUX_UNISTD_H

#include <sys/types.h>
/*#include <inttypes.h>*/

#define _SC_PAGE_SIZE 1
#define _SC_PAGESIZE 1

#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4

/* copied function prototypes from http://opengroup.org/onlinepubs/007908799/xsh/unistd.h.html */
/* functions marked with EXISTS have a (potentially incomplete) implimentation */
int          access(const char *, int);
unsigned int alarm(unsigned int);
int          brk(void *);
int          chdir(const char *);
int          chroot(const char *); /*(LEGACY)*/ /*EXISTS*/
int          chown(const char *, uid_t, gid_t);
int          close(int); /*EXISTS*/
size_t       confstr(int, char *, size_t);
char        *crypt(const char *, const char *);
char        *ctermid(char *);
char        *cuserid(char *s); /*(LEGACY)*/
int          dup(int); /*EXISTS*/
int          dup2(int, int); /*EXISTS*/
void         encrypt(char[64], int);
int          execl(const char *, const char *, ...);
int          execle(const char *, const char *, ...);
int          execlp(const char *, const char *, ...);
int          execv(const char *, char *const []);
int          execve(const char *, char *const [], char *const []);
int          execvp(const char *, char *const []);
void        _exit(int); /*EXISTS*/
int          fchown(int, uid_t, gid_t);
int          fchdir(int);
int          fdatasync(int);
pid_t        fork(void); /*EXISTS*/
long int     fpathconf(int, int);
int          fsync(int);
int          ftruncate(int, off_t);
char        *getcwd(char *, size_t);
int          getdtablesize(void); /*(LEGACY)*/
gid_t        getegid(void); /*EXISTS*/
uid_t        geteuid(void); /*EXISTS*/
gid_t        getgid(void); /*EXISTS*/
int          getgroups(int, gid_t []); /*EXISTS*/
long         gethostid(void);
char        *getlogin(void);
int          getlogin_r(char *, size_t);
int          getopt(int, char * const [], const char *);
int          getpagesize(void); /*(LEGACY)*/ /*EXISTS*/
char        *getpass(const char *); /*(LEGACY)*/
pid_t        getpgid(pid_t); /*EXISTS*/
pid_t        getpgrp(void); /*EXISTS*/
pid_t        getpid(void); /*EXISTS*/
pid_t        getppid(void); /*EXISTS*/
pid_t        getsid(pid_t);
uid_t        getuid(void); /*EXISTS*/
char        *getwd(char *);
int          isatty(int);
int          lchown(const char *, uid_t, gid_t);
int          link(const char *, const char *);
int          lockf(int, int, off_t);
off_t        lseek(int, off_t, int); /*EXISTS*/
int          nice(int);
long int     pathconf(const char *, int);
int          pause(void);
int          pipe(int [2]);
ssize_t      pread(int, void *, size_t, off_t); /*EXISTS*/
int          pthread_atfork(void (*)(void), void (*)(void), void(*)(void));
ssize_t      pwrite(int, const void *, size_t, off_t); /*EXISTS*/
ssize_t      read(int, void *, size_t); /*EXISTS*/
int          readlink(const char *, char *, size_t);
int          rmdir(const char *); /*EXISTS*/
/*void        *sbrk(intptr_t);*/
int          setgid(gid_t); /*EXISTS*/
int          setpgid(pid_t, pid_t);
pid_t        setpgrp(void);
int          setregid(gid_t, gid_t);
int          setreuid(uid_t, uid_t);
pid_t        setsid(void); /*EXISTS*/
int          setuid(uid_t); /*EXISTS*/
unsigned int sleep(unsigned int);
void         swab(const void *, void *, ssize_t);
int          symlink(const char *, const char *);
void         sync(void);
long int     sysconf(int); /*EXISTS*/
pid_t        tcgetpgrp(int);
int          tcsetpgrp(int, pid_t);
int          truncate(const char *, off_t);
char        *ttyname(int);
int          ttyname_r(int, char *, size_t);
useconds_t   ualarm(useconds_t, useconds_t);
int          unlink(const char *); /*EXISTS*/
int          usleep(useconds_t);
pid_t        vfork(void);
ssize_t      write(int, const void *, size_t); /*EXISTS*/

#endif

