#include <signal.h>
#include <stddef.h>

int sigaction(int sig, const struct sigaction *act, struct sigaction *oact)
{
	return 0;
}

int sigprocmask(int how, const sigset_t *set, sigset_t *oset)
{
	return 0;
}
