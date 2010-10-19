#ifndef _SIGNAL_H
#define _SIGNAL_H

#define sig_atomic_t int
typedef struct
{
	int num_sigs;
	int sigs[];
} sigset_t;

#define SIG_DFL 1
#define SIG_ERR 2
#define SIG_IGN 3

#define NULL 0
#define SIGABRT 1
#define SIGFPE 2
#define SIGILL 3
#define SIGINT 4
#define SIGSEGV 5
#define SIGTERM 6
#define SIGALRM 7
#define SIGTERM 8
#define SIGCHLD 9
#define SIGPIPE 10
#define SIGURG 11
#define SIGHUP 12
#define SIG_BLOCK 13
#define SIG_UNBLOCK 14

#define SIGMIN 0
#define SIGMAX 14
#define NSIG (SIGMAX + 1)

union sigval
{
	int sival_int;
	void *sival_ptr;
};

typedef struct
{
	int si_signo;
	int si_code;
	union sigval si_value;
} siginfo_t;

struct sigaction
{
	void (*sa_handler)(int);
	sigset_t sa_mask;
	int sa_flags;
	void (*sa_sigaction)(int, siginfo_t *, void *);
};

typedef struct
{
	void *ss_sp;
	size_t ss_size;
	int ss_flags;
} stack_t;

/*void (*signal(int sig, void (*func)(int)))(int);
int raise(int sig);*/

int sigaltstack(const stack_t *ss, stack_t *oss);
int sigfillset(sigset_t *set);
int sigaction(int sig, const struct sigaction *act, struct sigaction *oact);
int sigemptyset(sigset_t *set);
int sigaddset(sigset_t *set, int signo);
int sigprocmask(int how, const sigset_t *set, sigset_t *oset);

#endif
