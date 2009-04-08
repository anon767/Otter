#include <signal.h>
int sigfillset(sigset_t *set) {
	return 0;
}

int sigaction(int sig, const struct sigaction *restrict act,
							struct sigaction *restrict oact) {
	return 0;
}

int sigemptyset(sigset_t *set) {
	return 0;
}

int sigaddset(sigset_t *set, int signo) {
	return 0;
}

int sigprocmask(int how, const sigset_t *restrict set,
								sigset_t *restrict oset) {
	return 0;
}
