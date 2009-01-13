#include "../iosim.h"
#include <string.h>

int symtest_initialize() {
	// Make a 'file' for stdin
	IO_BUF* iosim_stdin = IOSIM_newbuf(-1,
"user anonymous
pass
help
list
site help
pasv
list
pwd
quit
");
//
//port 0,0,0,0,10,0
//list
//quit
//
//
//binary
//get README
//
//cd EXAMPLE
//pwd
//ls
//
//ascii
//get README
//
//delete README
//put README
//
//bye
//");
	IOSIM_attach(0,iosim_stdin);

	tunable_write_enable = __SYMBOLIC();
	tunable_anon_upload_enable = __SYMBOLIC();
	tunable_dirmessage_enable = __SYMBOLIC();
	//	tunable_connect_from_port_20 = __SYMBOLIC();
	tunable_run_as_launching_user = __SYMBOLIC();
	tunable_listen = __SYMBOLIC();

	tunable_ascii_download_enable = __SYMBOLIC();
	tunable_ascii_upload_enable = __SYMBOLIC();

	// Add the configuration file
//	char* confFileString = strdup(
//"write_enable=?
//anon_upload_enable=?
//dirmessage_enable=?
//connect_from_port_20=?
//nopriv_user=ft
//run_as_launching_user=?
//listen=?
//
//ascii_download_enable=?
//ascii_upload_enable=?
//");
//	IO_BUF* defaultConfFile = IOSIM_newbuf(-1, confFileString);
//	// Set '?'s to symbolic bytes
//	char *cp = strchr(confFileString,'?');
//	while (cp) {
//		*cp = __SYMBOLIC();
//		__ASSUME(*cp == '0' || *cp == '1'); // This only works properly if we --useLogicalOperators
//		cp = strchr(cp+1,'?');
//	}
//	IOSIM_addfile("/etc/vsftpd.conf",defaultConfFile);
	return 0;
}
