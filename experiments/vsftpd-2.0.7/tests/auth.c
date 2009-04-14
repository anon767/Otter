#include "iosim.h"
#include <string.h>
#include <stdlib.h>
#include "../tunables.h"
#include "symexe.h"

char **environ;

void symtest_initialize() {
	// Make the string of commands on fd 0
	char commandString[] = "auth
quit
";
	IOSIM_fd[0] = malloc(sizeof(sym_file_stream_t));
	IOSIM_fd[0]->offset = 0;
	IOSIM_fd[0]->sym_file = malloc(sizeof(sym_file_t));
	IOSIM_fd[0]->sym_file->contents = strdup(commandString);
	IOSIM_fd[0]->sym_file->stat.st_size = sizeof(commandString);
	IOSIM_fd[0]->sym_file->stat.st_mode = S_IFSOCK;

	// Make empty environ variable
	environ = malloc(sizeof(char*));
	environ[0] = NULL;

	// The symbolic executor can't currently handle multiple processes
	tunable_one_process_model = 1;

	// These are the integer flags
//	__SYMBOLIC(&tunable_accept_timeout);
//	__SYMBOLIC(&tunable_connect_timeout);
//	__SYMBOLIC(&tunable_local_umask);
//	__SYMBOLIC(&tunable_anon_umask);
//	__SYMBOLIC(&tunable_ftp_data_port);
//	__SYMBOLIC(&tunable_idle_session_timeout);
//	__SYMBOLIC(&tunable_data_connection_timeout);
//	//	__SYMBOLIC(&tunable_pasv_min_port); // Caused trouble with double arithmetic
//	//	__SYMBOLIC(&tunable_pasv_max_port); // Caused trouble with double arithmetic
//	__SYMBOLIC(&tunable_anon_max_rate);
//	__SYMBOLIC(&tunable_local_max_rate);
//	__SYMBOLIC(&tunable_listen_port);
//	__SYMBOLIC(&tunable_max_clients);
//	__SYMBOLIC(&tunable_file_open_mode);
//	__SYMBOLIC(&tunable_max_per_ip);
//	__SYMBOLIC(&tunable_trans_chunk_size);
//	__SYMBOLIC(&tunable_delay_failed_login);
//	__SYMBOLIC(&tunable_delay_successful_login);
//	__SYMBOLIC(&tunable_max_login_fails);
//	__SYMBOLIC(&tunable_chown_upload_mode);
//
//	// All flags from here down are boolean
//
//	__SYMBOLIC(&tunable_write_enable);
//	__SYMBOLIC(&tunable_anon_upload_enable);
//	__SYMBOLIC(&tunable_dirmessage_enable);
//	__SYMBOLIC(&tunable_ascii_upload_enable);
//	__SYMBOLIC(&tunable_ascii_download_enable);
//	__SYMBOLIC(&tunable_listen);
//	__SYMBOLIC(&tunable_run_as_launching_user);
//
//	__SYMBOLIC(&tunable_anonymous_enable);
//	__SYMBOLIC(&tunable_local_enable);
//	__SYMBOLIC(&tunable_pasv_enable);
//	__SYMBOLIC(&tunable_port_enable);
//	__SYMBOLIC(&tunable_chroot_local_user);
//	__SYMBOLIC(&tunable_anon_mkdir_write_enable);
//	__SYMBOLIC(&tunable_anon_other_write_enable);
//	__SYMBOLIC(&tunable_chown_uploads);
//	__SYMBOLIC(&tunable_connect_from_port_20);
//	__SYMBOLIC(&tunable_xferlog_enable);
//	__SYMBOLIC(&tunable_anon_world_readable_only);
//	__SYMBOLIC(&tunable_async_abor_enable);
//	__SYMBOLIC(&tunable_one_process_model);
//	__SYMBOLIC(&tunable_xferlog_std_format);
//	__SYMBOLIC(&tunable_pasv_promiscuous);
//	__SYMBOLIC(&tunable_deny_email_enable);
//	__SYMBOLIC(&tunable_chroot_list_enable);
//	__SYMBOLIC(&tunable_setproctitle_enable);
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
	__SYMBOLIC(&tunable_ssl_enable);
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

	return;
}
