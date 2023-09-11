#pragma no_other_abandoned

#include <unistd.h>

#define STR "hello"
#define LEN sizeof(STR)-1

int main() {
	__otter_libc_init();
    char filename[] = "a";
	int fd = open(filename, O_CREAT | O_RDWR, S_IRWXU);
	if (fork() == 0) {
		char buf[10];
		strcpy(buf, STR);
		int bytes_written = write(fd, buf, LEN);
		__ASSERT(bytes_written == LEN);
		int r = close(fd);
		__ASSERT(r == 0);
	} else {
		for (int i = 0; i < 150; i++); // Delay so that the write happens first
		lseek(fd, 0, SEEK_SET); //seek back to the beginning of the file
		char buf[] = "abcdefghi";
		int bytes_read = read(fd, buf, 10);
		__ASSERT(bytes_read == LEN);
		int cmp = strcmp(buf, "hellofghi");
		__ASSERT(cmp == 0);
		int r = close(fd);
		__ASSERT(r == 0);

	}
  return 0;
}
