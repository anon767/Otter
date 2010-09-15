int sigaltstack(const stack_t *ss, stack_t *oss)
{
  return 0;
}

int sigfillset(sigset_t *set)
{
	return 0;
}

int sigaction(int sig, const struct sigaction *act, struct sigaction *oact)
{
	return 0;
}

int sigemptyset(sigset_t *set)
{
	return 0;
}

int sigaddset(sigset_t *set, int signo)
{
	return 0;
}

int sigprocmask(int how, const sigset_t *set, sigset_t *oset)
{
	return 0;
}
