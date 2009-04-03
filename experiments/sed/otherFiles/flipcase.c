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

	sym_file_t* sed = IOSIM_addfile("flipcase.sed", 0);
	sed->contents = "s,\\([^A-Za-z]*\\)\\([A-Za-z]*\\),\\1\\L\\u\\2,g";
	sed->stat.st_size = 40;

	sym_file_t* input = IOSIM_addfile("flipcase.inp", 0);
	input->contents = "09 - 02 - 2002 00.00 Tg La7 La7 -\n09 - 02 - 2002 00.00 Brand New Tmc 2 -\n09 - 02 - 2002 00.10 Tg1 Notte Rai Uno -\n09 - 02 - 2002 00.15 Tg Parlamento Rai Due -\n09 - 02 - 2002 00.15 Kung Fu - La Leggenda Continua La7 -\n09 - 02 - 2002 00.20 Berserk - La CoNFESSIONE Di Gatz Italia 1 Cartoon\n09 - 02 - 2002 00.20 Tg3 - Tg3 Meteo Rai TrE -\n09 - 02 - 2002 00.25 Meteo 2 Rai Due -\n09 - 02 - 2002 00.30 Appuntamento Al CinEMA RaI Due -\n09 - 02 - 2002 00.30 Rai Educational - Mediamente Rai Tre -\n09 - 02 - 2002 00.35 Profiler Rai Due -\n09 - 02 - 2002 00.35 Stampa OggI - Che Tempo Fa Rai Uno -\n09 - 02 - 2002 00.45 Rai Educational - Babele: Euro Rai Uno -\n09 - 02 - 2002 00.45 BollettINO Della NEVE RETE 4 News\n09 - 02 - 2002 00.50 STUDIO Aperto - La Giornata Italia 1 News\n09 - 02 - 2002 00.50 BOCCA A Bocca - 2 Tempo Rete 4 Film\n09 - 02 - 2002 01.00 AppuntAMENTO Al Cinema Rai Tre -\n09 - 02 - 2002 01.00 Music NoN Stop Tmc 2 -\n09 - 02 - 2002 01.00 Studio SpORT Italia 1 SporT\n09 - 02 - 2002 01.00 Tg 5 - Notte Canale 5 News\n09 - 02 - 2002 01.05 Fuori Orario. CosE (Mai) Viste Rai Tre -\n09 - 02 - 2002 01.15 RAINOTTE Rai Due -\n09 - 02 - 2002 01.15 Sottovoce Rai Uno -\n09 - 02 - 2002 01.15 GiOCHI Olimpici InVERNALI - CERIMONIA Di Apertura Rai Tre -\n09 - 02 - 2002 01.17 Italia Interroga Rai Due -";
	input->stat.st_size = 1289;
}