#ifndef _SYS_WAIT_H
#define _SYS_WAIT_H

/* for waitid */
#define WNOHANG 1
#define WUNTRACED 2

#define WEXITSTATUS(s)  (s & 0x0001)
#define WIFCONTINUED(s) (s & 0x0002)
#define WIFEXITED(s)    (s & 0x0004)
#define WIFSIGNALED(s)  (s & 0x0008)
#define WIFSTOPPED(s)   (s & 0x0010)
#define WSTOPSIG(s)     (s & 0x0020)
#define WTERMSIG(s)     (s & 0x0040)

/* for options */
#define WEXITED 1
#define WSTOPPED 2
#define WCONTINUED 3
#define WNOHANG 4
#define WNOWAIT 5

#include <sys/types.h>
#include <signal.h>

enum id_type {P_ALL = -1, P_PID = -2, P_PGID = 0};

pid_t wait(int *stat_loc);
/*pid_t wait3(int *stat_loc, int options, struct rusage *resource_usage);*/
/*int waitid(idtype_t, id_t, siginfo_t *infop, int options);*/
pid_t waitpid(pid_t pid, int *stat_loc, int options);

#endif
