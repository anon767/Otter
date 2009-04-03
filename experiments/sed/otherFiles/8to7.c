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

	sym_file_t* sed = IOSIM_addfile("8to7.sed", 0);
	sed->contents = "l;d";
	sed->stat.st_size = 3;

	sym_file_t* input = IOSIM_addfile("8to7.inp", 0);
	input->contents = "�Ƥ�� ���ƻ��\n������ �߷�����\n���ε֤� ��Ŧ�ޤ���\n�ȴ֤��� ��餵��\n����ߤ� ���¤ι��\n������ʤ٤� ��줳�����\n�����ʤ٤� ��줳�� �¤�\n��ˤ����� ����\n�Ȥ��̾���";
	input->stat.st_size = 131;
}