#include<stdio.h>
#include<fcntl.h>
#include<unistd.h>

int __otter_libc_remove(const char* name)
{
	return unlink(name);
}
