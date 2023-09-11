#include "iosim.h"
#include <stdlib.h>

void symtest_initialize() {
	IOSIM_fd[1] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[1]->offset = 0;
	IOSIM_fd[1]->fd = 1;
	IOSIM_fd[1]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[1]->sym_file->contents = NULL;
	IOSIM_fd[1]->sym_file->stat.st_size = 0;
	stdout = IOSIM_fd[1];

	sym_file_t* sed = IOSIM_addfile("factor.sed", 0);
	sed->contents = "\ns/.*/&;9aaaaaaaaa8aaaaaaaa7aaaaaaa6aaaaaa5aaaaa4aaaa3aaa2aa1a0/\n:encode\ns/\\(a*\\)\\([0-9]\\)\\([0-9]*;.*\\2\\(a*\\)\\)/\\1\\1\\1\\1\\1\\1\\1\\1\\1\\1\\4\\3/\ntencode\ns/;.*//\n\nt7a\n\n:2\na\\\n2\nb2a\n:3\na\\\n3\nb3a\n:5\na\\\n5\nb5a\n:7\na\\\n7\n\n:7a\ns/^\\(aa*\\)\\1\\{6\\}$/\\1/\nt7\n:5a\ns/^\\(aa*\\)\\1\\{4\\}$/\\1/\nt5\n:3a\ns/^\\(aa*\\)\\1\\1$/\\1/\nt3\n:2a\ns/^\\(aa*\\)\\1$/\\1/\nt2\n\n/^a$/b\n\ns/^\\(aa*\\)\\1\\{10\\}/\\1=&/\n\n:factor\n/^\\(a\\{7,\\}\\)=\\1\\1*$/! {\n  # Decrement CANDIDATE, and search again if it is still >1\n  s/^a//\n  /^aa/b factor\n\n  # Print the last remaining factor: since it is stored in the NUMBER\n  # rather than in the CANDIDATE, swap 'em: now NUMBER=1\n  s/\\(.*\\)=\\(.*\\)/\\2=\\1/\n}\n\nh\ns/=.*/;;0a1aa2aaa3aaaa4aaaaa5aaaaaa6aaaaaaa7aaaaaaaa8aaaaaaaaa9/\n\n:decode\ns/^\\(a*\\)\\1\\{9\\}\\(a\\{0,9\\}\\)\\([0-9]*;.*[^a]\\2\\([0-9]\\)\\)/\\1\\4\\3/\n/^a/tdecode\ns/;.*//p\n\ng\n:divide\ns/^\\(a*\\)\\(=b*\\)\\1/\\1\\2b/\ntdivide\ny/b/a/\n\n/aa$/bfactor";
	sed->stat.st_size = 853;

	sym_file_t* input = IOSIM_addfile("factor.inp", 0);
	input->contents = "2\n3\n4\n5\n8\n11\n16\n143";
	input->stat.st_size = 19;
}