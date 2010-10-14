#pragma no_other_abandoned

#define STR "hello"
#define LEN sizeof(STR)-1

int main() {
	__otter_libc_init();
  char filename[] = "a";
	if (fork() == 0) {
		char buf[10];
		strcpy(buf, STR);
		int fd = open(filename, O_CREAT | O_WRONLY, S_IRWXU);
		int bytes_written = write(fd, buf, LEN);
		__ASSERT(bytes_written == LEN);
	} else {
		for (int i = 0; i < 150; i++); // Delay so that the write happens first
		char buf[] = "abcdefghi";;
		int fd = open(filename, O_RDONLY, S_IRWXU);
		int bytes_read = read(fd, buf, 10);
		__ASSERT(bytes_read == LEN);
		int cmp = strcmp(buf, "hellofghi");
		__ASSERT(cmp == 0);
	}
  return 0;
}
