#include<signal.h>

int __otter_libc_sigaltstack(const stack_t *ss, stack_t *oss)
{
  return 0;
}

int __otter_libc_sigfillset(sigset_t *set)
{
	return 0;
}

int __otter_libc_sigaction(int sig, const struct sigaction *act, struct sigaction *oact)
{
	return 0;
}

int __otter_libc_sigemptyset(sigset_t *set)
{
	return 0;
}

int __otter_libc_sigaddset(sigset_t *set, int signo)
{
	return 0;
}

int __otter_libc_sigprocmask(int how, const sigset_t *set, sigset_t *oset)
{
	return 0;
}
