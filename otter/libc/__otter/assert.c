void __otter_libc_failwith(char *msg)
{
	__EVALSTR(msg, 500);
	exit();
}