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

	sym_file_t* sed = IOSIM_addfile("khadafy.sed", 0);
	sed->contents = "/M[ou]'\\{0,1\\}am\\{1,2\\}[ae]r .*\\([AEae]l[- ]\\)\\{0,1\\}[GKQ]h\\{0,1\\}[aeu]\\{1,\\}\\([dtz][dhz]\\{0,1\\}\\)\\{1,\\}af[iy]/!d";
	sed->stat.st_size = 113;

	sym_file_t* input = IOSIM_addfile("khadafy.inp", 0);
	input->contents = "1)  Muammar Qaddafi\n2)  Mo'ammar Gadhafi\n3)  Muammar Kaddafi\n4)  Muammar Qadhafi\n5)  Moammar El Kadhafi\n6)  Muammar Gadafi\n7)  Mu'ammar al-Qadafi\n8)  Moamer El Kazzafi\n9)  Moamar al-Gaddafi\n10) Mu'ammar Al Qathafi\n11) Muammar Al Qathafi\n12) Mo'ammar el-Gadhafi\n13) Moamar El Kadhafi\n14) Muammar al-Qadhafi\n15) Mu'ammar al-Qadhdhafi\n16) Mu'ammar Qadafi\n17) Moamar Gaddafi\n18) Mu'ammar Qadhdhafi\n19) Muammar Khaddafi\n20) Muammar al-Khaddafi\n21) Mu'amar al-Kadafi\n22) Muammar Ghaddafy\n23) Muammar Ghadafi\n24) Muammar Ghaddafi\n25) Muamar Kaddafi\n26) Muammar Quathafi\n27) Muammar Gheddafi\n28) Muamar Al-Kaddafi\n29) Moammar Khadafy \n30) Moammar Qudhafi\n31) Mu'ammar al-Qaddafi\n32) Mulazim Awwal Mu'ammar Muhammad Abu Minyar al-Qadhafi";
	input->stat.st_size = 728;
}