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

	sym_file_t* sed = IOSIM_addfile("distrib.sed", 0);
	sed->contents = "\n1i\\\nPath: mailnewsgateway\n	:a\n	/^[Rr]eceived:/b r\n	/^[Nn]ewsgroups:/b r\n	/^[Pp]ath:/b r\n	/^[Tt][Oo]:/s/^/Original-/\n	/^[Cc][Cc]:/s/^/Original-/\n	/^[Rr][Ee][Ss][Ee][Nn][Tt]-.*/s/^/Original-/\n	/^[Mm][Ee][Ss][Ss][Aa][Gg][Ee]-[Ii][Dd]:/s/@/.alt.buddha.fat.short.guy@/\n	s/^[Ii]n-[Rr]eply-[Tt]o:/References:/\n	/^From:/{\n		s/<\\([^@]*\\)>$/<\\1@$thissite>/\n		s/^From:[ 	][	]*\\(.*\\)  *<\\(.*\\)>$/From: \\2 (\\1)/\n		}\n	s/-[Ii]d:/-ID:/\n	s/^[Ss][Uu][Bb][Jj][Ee][Cc][Tt]:[ 	]*$/Subject: (none)/\n	s/^\\([^:]*:\\)[	 ]*/\\1 /\n	/^$/{i\\\nNewsgroups: alt.buddha.short.fat.guy\\\nDistribution: world\\\nSender: news@cygnus.com\\\nApproved: alt.buddha.short.fat.guy@cygnus.com\n	b e\n	}\n	p\n	n\n	b a\n	:r\n	s/.*//g\n	n\n	/^[ 	]/b r\n	b a\n	:e\n	p\n	n\n	b e";
	sed->stat.st_size = 708;

	sym_file_t* input = IOSIM_addfile("distrib.inp", 0);
	input->contents = "From crash@cygnus.com  Wed Mar  8 18:02:42 1995\nReceived: from s1.msi.umn.edu (s1.msi.umn.edu [128.101.24.1]) by cygnus.com (8.6.9/8.6.9) with ESMTP id SAA21692 for <crash@cygnus.com>; Wed, 8 Mar 1995 18:02:41 -0800\nReceived: from cygint.cygnus.com (cygint.cygnus.com [140.174.1.1]) by s1.msi.umn.edu (8.6.10/8.6.9) with ESMTP id TAA13398 for <molenda@msi.umn.edu>; Wed, 8 Mar 1995 19:59:18 -0600\nReceived: from phydeaux.cygnus.com (phydeaux.cygnus.com [140.174.1.85]) by cygnus.com (8.6.9/8.6.9) with SMTP id SAA21688 for <molenda@msi.umn.edu>; Wed, 8 Mar 1995 18:02:33 -0800\nFrom: Jason Molenda <crash@cygnus.com>\nReceived: by phydeaux.cygnus.com (5.65/4.7) id AA06931; Wed, 8 Mar 1995 18:02:28 -0800\nMessage-Id: <9503090202.AA06931@phydeaux.cygnus.com>\nSubject: Note for sed testsuite\nTo: molenda@msi.umn.edu\nDate: Wed, 8 Mar 1995 18:02:24 -0800 (PST)\nX-Mailer: ELM [version 2.4 PL23]\n\n                _Summum Bonum_\n\n    All the breath and the bloom of the\n            year in the bag of one bee:\n    All the wonder and wealth of the mine in\n         the heart of one gem:\n    In the core of one pearl all the shade and the\n           shine of the sea:\n    Breath and bloom, shade and shine, -- wonder,\n        wealth, and -- how far above them --\n          Truth, thats brighter than gem,\n          Trust, that's purer than pearl, --\n    Brightest truth, purest trust in the universe --\n              all were for me\n                 In the kiss of one girl.\n        -- Robert Browning";
	input->stat.st_size = 1490;
}