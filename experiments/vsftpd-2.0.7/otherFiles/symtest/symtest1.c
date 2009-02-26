#include "../iosim.h"
//#include <string.h>
#include <stdlib.h>

char **environ;

void symtest_initialize() {
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

	// Make files for stdout and stderr
	char *buf = malloc(1<<20);
	IO_BUF* iosim_stdout = IOSIM_newbuf(1<<20,buf);
	IOSIM_attach(1,iosim_stdout);
	buf = malloc(1<<20);
	IO_BUF* iosim_stderr = IOSIM_newbuf(1<<20,buf);
	IOSIM_attach(2,iosim_stderr);

	// Make empty environ variable
	environ = malloc(sizeof(char*));
	environ[0] = NULL;

	// These are the integer flags
	__SYMBOLIC(&tunable_accept_timeout);
	__SYMBOLIC(&tunable_connect_timeout);
	__SYMBOLIC(&tunable_local_umask);
	__SYMBOLIC(&tunable_anon_umask);
	__SYMBOLIC(&tunable_ftp_data_port);
	__SYMBOLIC(&tunable_idle_session_timeout);
	__SYMBOLIC(&tunable_data_connection_timeout);
	//	__SYMBOLIC(&tunable_pasv_min_port); // Caused trouble with double arithmetic
	//	__SYMBOLIC(&tunable_pasv_max_port); // Caused trouble with double arithmetic
	__SYMBOLIC(&tunable_anon_max_rate);
	__SYMBOLIC(&tunable_local_max_rate);
	__SYMBOLIC(&tunable_listen_port);
	__SYMBOLIC(&tunable_max_clients);
	__SYMBOLIC(&tunable_file_open_mode);
	__SYMBOLIC(&tunable_max_per_ip);
	__SYMBOLIC(&tunable_trans_chunk_size);
	__SYMBOLIC(&tunable_delay_failed_login);
	__SYMBOLIC(&tunable_delay_successful_login);
	__SYMBOLIC(&tunable_max_login_fails);
	__SYMBOLIC(&tunable_chown_upload_mode);
// Setting everything to here symbolic took 5 seconds and required 3 runs to cover

	// All flags from here down are boolean

	// These 7 flags were symbolic in my first test
	__SYMBOLIC(&tunable_write_enable);
	__SYMBOLIC(&tunable_anon_upload_enable);
	__SYMBOLIC(&tunable_dirmessage_enable);
	__SYMBOLIC(&tunable_ascii_upload_enable);
	__SYMBOLIC(&tunable_ascii_download_enable);
	__SYMBOLIC(&tunable_listen);
	__SYMBOLIC(&tunable_run_as_launching_user);

	// Setting everything to here symbolic took 1m 43s and required 3 runs to cover
	__SYMBOLIC(&tunable_anonymous_enable);
	__SYMBOLIC(&tunable_local_enable);
	__SYMBOLIC(&tunable_pasv_enable);
	__SYMBOLIC(&tunable_port_enable);
	__SYMBOLIC(&tunable_chroot_local_user);
//	// Setting everything to here symbolic took 7m 4s and required 5 runs to cover (but there were errors)
//	__SYMBOLIC(&tunable_anon_mkdir_write_enable);
//	__SYMBOLIC(&tunable_anon_other_write_enable);
//	__SYMBOLIC(&tunable_chown_uploads);
//	__SYMBOLIC(&tunable_connect_from_port_20);
//	__SYMBOLIC(&tunable_xferlog_enable);
//	// Setting everything to here symbolic ran out of memory after 96m 30s
//	__SYMBOLIC(&tunable_anon_world_readable_only);
//	__SYMBOLIC(&tunable_async_abor_enable);
//	__SYMBOLIC(&tunable_one_process_model);
//	__SYMBOLIC(&tunable_xferlog_std_format);
//	__SYMBOLIC(&tunable_pasv_promiscuous);
//	__SYMBOLIC(&tunable_deny_email_enable);
//	__SYMBOLIC(&tunable_chroot_list_enable);
//	__SYMBOLIC(&tunable_setproctitle_enable); // Uses external environ variable
//	__SYMBOLIC(&tunable_text_userdb_names);
//	__SYMBOLIC(&tunable_ls_recurse_enable);
//	__SYMBOLIC(&tunable_log_ftp_protocol);
//	__SYMBOLIC(&tunable_guest_enable);
//	__SYMBOLIC(&tunable_userlist_enable);
//	__SYMBOLIC(&tunable_userlist_deny);
//	__SYMBOLIC(&tunable_use_localtime);
//	__SYMBOLIC(&tunable_check_shell);
//	__SYMBOLIC(&tunable_hide_ids);
//	__SYMBOLIC(&tunable_port_promiscuous);
//	__SYMBOLIC(&tunable_passwd_chroot_enable);
//	__SYMBOLIC(&tunable_no_anon_password);
//	__SYMBOLIC(&tunable_tcp_wrappers);
//	__SYMBOLIC(&tunable_use_sendfile);
//	__SYMBOLIC(&tunable_force_dot_files);
//	__SYMBOLIC(&tunable_listen_ipv6);
//	__SYMBOLIC(&tunable_dual_log_enable);
//	__SYMBOLIC(&tunable_syslog_enable);
//	__SYMBOLIC(&tunable_background);
//	__SYMBOLIC(&tunable_virtual_use_local_privs);
//	__SYMBOLIC(&tunable_session_support);
//	__SYMBOLIC(&tunable_download_enable);
//	__SYMBOLIC(&tunable_dirlist_enable);
//	__SYMBOLIC(&tunable_chmod_enable);
//	__SYMBOLIC(&tunable_secure_email_list_enable);
//	__SYMBOLIC(&tunable_no_log_lock);
//	__SYMBOLIC(&tunable_ssl_enable);
//	__SYMBOLIC(&tunable_allow_anon_ssl);
//	__SYMBOLIC(&tunable_force_local_logins_ssl);
//	__SYMBOLIC(&tunable_force_local_data_ssl);
//	__SYMBOLIC(&tunable_sslv2);
//	__SYMBOLIC(&tunable_sslv3);
//	__SYMBOLIC(&tunable_tlsv1);
//	__SYMBOLIC(&tunable_tilde_user_enable);
//	__SYMBOLIC(&tunable_force_anon_logins_ssl);
//	__SYMBOLIC(&tunable_force_anon_data_ssl);
//	__SYMBOLIC(&tunable_mdtm_write);
//	__SYMBOLIC(&tunable_lock_upload_files);
//	__SYMBOLIC(&tunable_pasv_addr_resolve);
//	__SYMBOLIC(&tunable_debug_ssl);
//	__SYMBOLIC(&tunable_require_cert);
//	__SYMBOLIC(&tunable_validate_cert);
//	__SYMBOLIC(&tunable_strict_ssl_read_eof);
//	__SYMBOLIC(&tunable_strict_ssl_write_shutdown);
//	__SYMBOLIC(&tunable_ssl_request_cert);
//	__SYMBOLIC(&tunable_delete_failed_uploads);


//	char* messageFileString = strdup(
//"what is the banner message
//and what is the next line");
//	IO_BUF* messageFile = IOSIM_newbuf(-1, messageFileString);
//	IOSIM_addfile(".message",messageFile);


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
	return;
}
