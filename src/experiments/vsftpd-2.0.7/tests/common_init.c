#include "iosim.h"

extern void common_initialization(const char*);
#include <string.h>
#include <stdlib.h>
#include "../tunables.h"
#include "symexe.h"

char **environ;

// Create a new (empty) sym_file_stream_t. If fd >= 0, attach the
// stream at file descriptor fd.
sym_file_stream_t *newStream(int fd) {
	sym_file_stream_t *stream = malloc(sizeof(sym_file_stream_t));
	stream->offset = 0;
	stream->sym_file = malloc(sizeof(sym_file_t));
	stream->sym_file->contents = NULL;
	stream->sym_file->stat.st_size = 0;
	stream->buffer = NULL;
	if (fd >= 0) {
		stream->fd = fd;
		IOSIM_fd[fd] = stream;
	}
	return stream;
}

void common_initialization(const char *commandString) {
	// Make stdout and stderr
	newStream(1);
	newStream(2);

	// vsftpd reads its commands either from stdin (fd 0) or from fd 5,
	// depending on the configuration. I modified it so that it always
	// reads its commands from fd 5.
	newStream(5);
	int bufferLen = strlen(commandString); // No '1 +' because we don't need to include the terminating null
	IOSIM_fd[5]->sym_file->contents = memcpy(malloc(bufferLen),commandString,bufferLen);
	IOSIM_fd[5]->sym_file->stat.st_size = bufferLen;
	IOSIM_fd[5]->sym_file->stat.st_mode = S_IFSOCK;

	// From Otter's perspective, a PORT command increases the fd number by 2 and a PASV increases it by 3.
	// Here, we just initialize a bunch of file descriptors.
	for (int i = 6; i <= 25; ++i) {
		newStream(i);
	}

	// For files, we don't include the terminating null, so we just copy strlen() bytes (not 1+strlen())
#define ADDFILE(filename,contents) IOSIM_addfile((filename),(contents),strlen((contents)),0)

	// Create some files in the file system
	ADDFILE("/file1","abc");
	ADDFILE("/file2","abc");
	ADDFILE("/file3","def");
	ADDFILE("/ftp/file4","jfwoeifj\nweofjaiwe");
	char x[] = {6,2,78,250,9,100,17,0,111};
	IOSIM_addfile("/ftp/file5",x,sizeof(x),0);
	ADDFILE("/ftp/file6","junk");
	ADDFILE("/ftp/.hidden","boo!");
	char y[0];
	IOSIM_addfile("/ftp/emptyFile",y,0,0);
	ADDFILE("/ftp/123/654","\b\b\b\b\b\t");
	ADDFILE("/ftp/123/.hidden","this file is hidden\n");
	ADDFILE("/dir/file1","38 2qu938 30r9 u3");
	ADDFILE("/dir/file2","       ");
	ADDFILE("/dir/file3","\r\r\r\r\n    \t\t\t x");
	ADDFILE("/dir/file4","oaihwefaj");
	ADDFILE("/dir/file5","joi \\\"joewijf");

	// Make empty environ variable
	environ = malloc(sizeof(char*));
	environ[0] = NULL;

	// Initialize the configuration options

	// The symbolic executor can't currently handle multiple processes
	tunable_one_process_model = 1;

	// These are the integer flags

#ifdef TUNABLE_ACCEPT_TIMEOUT
	  tunable_accept_timeout = TUNABLE_ACCEPT_TIMEOUT;
#else
	  __SYMBOLIC(&tunable_accept_timeout);
#endif
#ifdef TUNABLE_CONNECT_TIMEOUT
	  tunable_connect_timeout = TUNABLE_CONNECT_TIMEOUT;
#else
	  __SYMBOLIC(&tunable_connect_timeout);
#endif
#ifdef TUNABLE_LOCAL_UMASK
	  tunable_local_umask = TUNABLE_LOCAL_UMASK;
#else
	  __SYMBOLIC(&tunable_local_umask);
#endif
#ifdef TUNABLE_ANON_UMASK
	  tunable_anon_umask = TUNABLE_ANON_UMASK;
#else
	//  __SYMBOLIC(&tunable_anon_umask); // Too much branching when printing file permissions
#endif
#ifdef TUNABLE_FTP_DATA_PORT
	  tunable_ftp_data_port = TUNABLE_FTP_DATA_PORT;
#else
	  __SYMBOLIC(&tunable_ftp_data_port);
#endif
#ifdef TUNABLE_IDLE_SESSION_TIMEOUT
	  tunable_idle_session_timeout = TUNABLE_IDLE_SESSION_TIMEOUT;
#else
	//  __SYMBOLIC(&tunable_idle_session_timeout); // This value gets printed to the screen by a STAT command
#endif
#ifdef TUNABLE_DATA_CONNECTION_TIMEOUT
	  tunable_data_connection_timeout = TUNABLE_DATA_CONNECTION_TIMEOUT;
#else
	  __SYMBOLIC(&tunable_data_connection_timeout);
#endif
#ifdef TUNABLE_PASV_MIN_PORT
	  tunable_pasv_min_port = TUNABLE_PASV_MIN_PORT;
#else
	//  __SYMBOLIC(&tunable_pasv_min_port); // Caused trouble with double arithmetic
#endif
#ifdef TUNABLE_PASV_MAX_PORT
	  tunable_pasv_max_port = TUNABLE_PASV_MAX_PORT;
#else
	//  __SYMBOLIC(&tunable_pasv_max_port); // Caused trouble with double arithmetic
#endif
#ifdef TUNABLE_ANON_MAX_RATE
	  tunable_anon_max_rate = TUNABLE_ANON_MAX_RATE;
#else
	//  __SYMBOLIC(&tunable_anon_max_rate); // This value gets printed to the screen by a STAT command
#endif
#ifdef TUNABLE_LOCAL_MAX_RATE
	  tunable_local_max_rate = TUNABLE_LOCAL_MAX_RATE;
#else
	  __SYMBOLIC(&tunable_local_max_rate);
#endif
#ifdef TUNABLE_LISTEN_PORT
	  tunable_listen_port = TUNABLE_LISTEN_PORT;
#else
	  __SYMBOLIC(&tunable_listen_port);
#endif
#ifdef TUNABLE_MAX_CLIENTS
	  tunable_max_clients = TUNABLE_MAX_CLIENTS;
#else
	  __SYMBOLIC(&tunable_max_clients);
#endif
#ifdef TUNABLE_FILE_OPEN_MODE
	  tunable_file_open_mode = TUNABLE_FILE_OPEN_MODE;
#else
	//  __SYMBOLIC(&tunable_file_open_mode); // Too much branching when printing file permissions
#endif
#ifdef TUNABLE_MAX_PER_IP
	  tunable_max_per_ip = TUNABLE_MAX_PER_IP;
#else
	  __SYMBOLIC(&tunable_max_per_ip);
#endif
#ifdef TUNABLE_TRANS_CHUNK_SIZE
	  tunable_trans_chunk_size = TUNABLE_TRANS_CHUNK_SIZE;
#else
	  __SYMBOLIC(&tunable_trans_chunk_size);
#endif
#ifdef TUNABLE_DELAY_FAILED_LOGIN
	  tunable_delay_failed_login = TUNABLE_DELAY_FAILED_LOGIN;
#else
	  __SYMBOLIC(&tunable_delay_failed_login);
#endif
#ifdef TUNABLE_DELAY_SUCCESSFUL_LOGIN
	  tunable_delay_successful_login = TUNABLE_DELAY_SUCCESSFUL_LOGIN;
#else
	  __SYMBOLIC(&tunable_delay_successful_login);
#endif
#ifdef TUNABLE_MAX_LOGIN_FAILS
	  tunable_max_login_fails = TUNABLE_MAX_LOGIN_FAILS;
#else
	  __SYMBOLIC(&tunable_max_login_fails);
#endif
#ifdef TUNABLE_CHOWN_UPLOAD_MODE
	  tunable_chown_upload_mode = TUNABLE_CHOWN_UPLOAD_MODE;
#else
	  __SYMBOLIC(&tunable_chown_upload_mode);
#endif

		// All flags from here down are boolean

#ifdef TUNABLE_WRITE_ENABLE
	  tunable_write_enable = TUNABLE_WRITE_ENABLE;
#else
	  __SYMBOLIC(&tunable_write_enable);
	  __ASSUME(OR((tunable_write_enable == 0),(tunable_write_enable == 1)));
#endif
#ifdef TUNABLE_ANON_UPLOAD_ENABLE
	  tunable_anon_upload_enable = TUNABLE_ANON_UPLOAD_ENABLE;
#else
	  __SYMBOLIC(&tunable_anon_upload_enable);
	  __ASSUME(OR((tunable_anon_upload_enable == 0),(tunable_anon_upload_enable == 1)));
#endif
#ifdef TUNABLE_DIRMESSAGE_ENABLE
	  tunable_dirmessage_enable = TUNABLE_DIRMESSAGE_ENABLE;
#else
	  __SYMBOLIC(&tunable_dirmessage_enable);
	  __ASSUME(OR((tunable_dirmessage_enable == 0),(tunable_dirmessage_enable == 1)));
#endif
#ifdef TUNABLE_ASCII_UPLOAD_ENABLE
	  tunable_ascii_upload_enable = TUNABLE_ASCII_UPLOAD_ENABLE;
#else
	  __SYMBOLIC(&tunable_ascii_upload_enable);
	  __ASSUME(OR((tunable_ascii_upload_enable == 0),(tunable_ascii_upload_enable == 1)));
#endif
#ifdef TUNABLE_ASCII_DOWNLOAD_ENABLE
	  tunable_ascii_download_enable = TUNABLE_ASCII_DOWNLOAD_ENABLE;
#else
	  __SYMBOLIC(&tunable_ascii_download_enable);
	  __ASSUME(OR((tunable_ascii_download_enable == 0),(tunable_ascii_download_enable == 1)));
#endif
#ifdef TUNABLE_LISTEN
	  tunable_listen = TUNABLE_LISTEN;
#else
	  __SYMBOLIC(&tunable_listen);
	  __ASSUME(OR((tunable_listen == 0),(tunable_listen == 1)));
#endif
#ifdef TUNABLE_RUN_AS_LAUNCHING_USER
	  tunable_run_as_launching_user = TUNABLE_RUN_AS_LAUNCHING_USER;
#else
	  __SYMBOLIC(&tunable_run_as_launching_user);
	  __ASSUME(OR((tunable_run_as_launching_user == 0),(tunable_run_as_launching_user == 1)));
#endif

#ifdef TUNABLE_ANONYMOUS_ENABLE
	  tunable_anonymous_enable = TUNABLE_ANONYMOUS_ENABLE;
#else
	  __SYMBOLIC(&tunable_anonymous_enable);
	  __ASSUME(OR((tunable_anonymous_enable == 0),(tunable_anonymous_enable == 1)));
#endif
#ifdef TUNABLE_LOCAL_ENABLE
	  tunable_local_enable = TUNABLE_LOCAL_ENABLE;
#else
	  __SYMBOLIC(&tunable_local_enable);
	  __ASSUME(OR((tunable_local_enable == 0),(tunable_local_enable == 1)));
#endif
#ifdef TUNABLE_PASV_ENABLE
	  tunable_pasv_enable = TUNABLE_PASV_ENABLE;
#else
	//  __SYMBOLIC(&tunable_pasv_enable);
	//  __ASSUME(OR((tunable_pasv_enable == 0),(tunable_pasv_enable == 1)));
#endif
#ifdef TUNABLE_PORT_ENABLE
	  tunable_port_enable = TUNABLE_PORT_ENABLE;
#else
	//  __SYMBOLIC(&tunable_port_enable);
	//  __ASSUME(OR((tunable_port_enable == 0),(tunable_port_enable == 1)));
#endif
#ifdef TUNABLE_CHROOT_LOCAL_USER
	  tunable_chroot_local_user = TUNABLE_CHROOT_LOCAL_USER;
#else
	  __SYMBOLIC(&tunable_chroot_local_user);
	  __ASSUME(OR((tunable_chroot_local_user == 0),(tunable_chroot_local_user == 1)));
#endif
#ifdef TUNABLE_ANON_MKDIR_WRITE_ENABLE
	  tunable_anon_mkdir_write_enable = TUNABLE_ANON_MKDIR_WRITE_ENABLE;
#else
	  __SYMBOLIC(&tunable_anon_mkdir_write_enable);
	  __ASSUME(OR((tunable_anon_mkdir_write_enable == 0),(tunable_anon_mkdir_write_enable == 1)));
#endif
#ifdef TUNABLE_ANON_OTHER_WRITE_ENABLE
	  tunable_anon_other_write_enable = TUNABLE_ANON_OTHER_WRITE_ENABLE;
#else
	  __SYMBOLIC(&tunable_anon_other_write_enable);
	  __ASSUME(OR((tunable_anon_other_write_enable == 0),(tunable_anon_other_write_enable == 1)));
#endif
#ifdef TUNABLE_CHOWN_UPLOADS
	  tunable_chown_uploads = TUNABLE_CHOWN_UPLOADS;
#else
	//  __SYMBOLIC(&tunable_chown_uploads);
	//  __ASSUME(OR((tunable_chown_uploads == 0),(tunable_chown_uploads == 1)));
#endif
#ifdef TUNABLE_CONNECT_FROM_PORT_20
	  tunable_connect_from_port_20 = TUNABLE_CONNECT_FROM_PORT_20;
#else
	//  __SYMBOLIC(&tunable_connect_from_port_20);
	//  __ASSUME(OR((tunable_connect_from_port_20 == 0),(tunable_connect_from_port_20 == 1)));
#endif
#ifdef TUNABLE_XFERLOG_ENABLE
	  tunable_xferlog_enable = TUNABLE_XFERLOG_ENABLE;
#else
	//  __SYMBOLIC(&tunable_xferlog_enable);
	//  __ASSUME(OR((tunable_xferlog_enable == 0),(tunable_xferlog_enable == 1)));
#endif
#ifdef TUNABLE_ANON_WORLD_READABLE_ONLY
	  tunable_anon_world_readable_only = TUNABLE_ANON_WORLD_READABLE_ONLY;
#else
	//  __SYMBOLIC(&tunable_anon_world_readable_only);
	//  __ASSUME(OR((tunable_anon_world_readable_only == 0),(tunable_anon_world_readable_only == 1)));
#endif
#ifdef TUNABLE_ASYNC_ABOR_ENABLE
	  tunable_async_abor_enable = TUNABLE_ASYNC_ABOR_ENABLE;
#else
	//  __SYMBOLIC(&tunable_async_abor_enable);
	//  __ASSUME(OR((tunable_async_abor_enable == 0),(tunable_async_abor_enable == 1)));
#endif
#ifdef TUNABLE_XFERLOG_STD_FORMAT
	  tunable_xferlog_std_format = TUNABLE_XFERLOG_STD_FORMAT;
#else
	//  __SYMBOLIC(&tunable_xferlog_std_format);
	//  __ASSUME(OR((tunable_xferlog_std_format == 0),(tunable_xferlog_std_format == 1)));
#endif
#ifdef TUNABLE_PASV_PROMISCUOUS
	  tunable_pasv_promiscuous = TUNABLE_PASV_PROMISCUOUS;
#else
	//  __SYMBOLIC(&tunable_pasv_promiscuous);
	//  __ASSUME(OR((tunable_pasv_promiscuous == 0),(tunable_pasv_promiscuous == 1)));
#endif
#ifdef TUNABLE_DENY_EMAIL_ENABLE
	  tunable_deny_email_enable = TUNABLE_DENY_EMAIL_ENABLE;
#else
	//  __SYMBOLIC(&tunable_deny_email_enable);
	//  __ASSUME(OR((tunable_deny_email_enable == 0),(tunable_deny_email_enable == 1)));
#endif
#ifdef TUNABLE_CHROOT_LIST_ENABLE
	  tunable_chroot_list_enable = TUNABLE_CHROOT_LIST_ENABLE;
#else
	//  __SYMBOLIC(&tunable_chroot_list_enable);
	//  __ASSUME(OR((tunable_chroot_list_enable == 0),(tunable_chroot_list_enable == 1)));
#endif
#ifdef TUNABLE_SETPROCTITLE_ENABLE
	  tunable_setproctitle_enable = TUNABLE_SETPROCTITLE_ENABLE;
#else
	  __SYMBOLIC(&tunable_setproctitle_enable);
	  __ASSUME(OR((tunable_setproctitle_enable == 0),(tunable_setproctitle_enable == 1)));
#endif
#ifdef TUNABLE_TEXT_USERDB_NAMES
	  tunable_text_userdb_names = TUNABLE_TEXT_USERDB_NAMES;
#else
	//  __SYMBOLIC(&tunable_text_userdb_names);
	//  __ASSUME(OR((tunable_text_userdb_names == 0),(tunable_text_userdb_names == 1)));
#endif
#ifdef TUNABLE_LS_RECURSE_ENABLE
	  tunable_ls_recurse_enable = TUNABLE_LS_RECURSE_ENABLE;
#else
	//  __SYMBOLIC(&tunable_ls_recurse_enable);
	//  __ASSUME(OR((tunable_ls_recurse_enable == 0),(tunable_ls_recurse_enable == 1)));
#endif
#ifdef TUNABLE_LOG_FTP_PROTOCOL
	  tunable_log_ftp_protocol = TUNABLE_LOG_FTP_PROTOCOL;
#else
	//  __SYMBOLIC(&tunable_log_ftp_protocol);
	//  __ASSUME(OR((tunable_log_ftp_protocol == 0),(tunable_log_ftp_protocol == 1)));
#endif
#ifdef TUNABLE_GUEST_ENABLE
	  tunable_guest_enable = TUNABLE_GUEST_ENABLE;
#else
	//  __SYMBOLIC(&tunable_guest_enable);
	//  __ASSUME(OR((tunable_guest_enable == 0),(tunable_guest_enable == 1)));
#endif
#ifdef TUNABLE_USERLIST_ENABLE
	  tunable_userlist_enable = TUNABLE_USERLIST_ENABLE;
#else
	//  __SYMBOLIC(&tunable_userlist_enable);
	//  __ASSUME(OR((tunable_userlist_enable == 0),(tunable_userlist_enable == 1)));
#endif
#ifdef TUNABLE_USERLIST_DENY
	  tunable_userlist_deny = TUNABLE_USERLIST_DENY;
#else
	//  __SYMBOLIC(&tunable_userlist_deny);
	//  __ASSUME(OR((tunable_userlist_deny == 0),(tunable_userlist_deny == 1)));
#endif
#ifdef TUNABLE_USE_LOCALTIME
	  tunable_use_localtime = TUNABLE_USE_LOCALTIME;
#else
	//  __SYMBOLIC(&tunable_use_localtime);
	//  __ASSUME(OR((tunable_use_localtime == 0),(tunable_use_localtime == 1)));
#endif
#ifdef TUNABLE_CHECK_SHELL
	  tunable_check_shell = TUNABLE_CHECK_SHELL;
#else
	//  __SYMBOLIC(&tunable_check_shell);
	//  __ASSUME(OR((tunable_check_shell == 0),(tunable_check_shell == 1)));
#endif
#ifdef TUNABLE_HIDE_IDS
	  tunable_hide_ids = TUNABLE_HIDE_IDS;
#else
	//  __SYMBOLIC(&tunable_hide_ids);
	//  __ASSUME(OR((tunable_hide_ids == 0),(tunable_hide_ids == 1)));
#endif
#ifdef TUNABLE_PORT_PROMISCUOUS
	  tunable_port_promiscuous = TUNABLE_PORT_PROMISCUOUS;
#else
	  __SYMBOLIC(&tunable_port_promiscuous);
	  __ASSUME(OR((tunable_port_promiscuous == 0),(tunable_port_promiscuous == 1)));
#endif
#ifdef TUNABLE_PASSWD_CHROOT_ENABLE
	  tunable_passwd_chroot_enable = TUNABLE_PASSWD_CHROOT_ENABLE;
#else
	//  __SYMBOLIC(&tunable_passwd_chroot_enable);
	//  __ASSUME(OR((tunable_passwd_chroot_enable == 0),(tunable_passwd_chroot_enable == 1)));
#endif
#ifdef TUNABLE_NO_ANON_PASSWORD
	  tunable_no_anon_password = TUNABLE_NO_ANON_PASSWORD;
#else
	//  __SYMBOLIC(&tunable_no_anon_password);
	//  __ASSUME(OR((tunable_no_anon_password == 0),(tunable_no_anon_password == 1)));
#endif
#ifdef TUNABLE_TCP_WRAPPERS
	  tunable_tcp_wrappers = TUNABLE_TCP_WRAPPERS;
#else
	//  __SYMBOLIC(&tunable_tcp_wrappers);
	//  __ASSUME(OR((tunable_tcp_wrappers == 0),(tunable_tcp_wrappers == 1)));
#endif
#ifdef TUNABLE_USE_SENDFILE
	  tunable_use_sendfile = TUNABLE_USE_SENDFILE;
#else
	//  __SYMBOLIC(&tunable_use_sendfile);
	//  __ASSUME(OR((tunable_use_sendfile == 0),(tunable_use_sendfile == 1)));
#endif
#ifdef TUNABLE_FORCE_DOT_FILES
	  tunable_force_dot_files = TUNABLE_FORCE_DOT_FILES;
#else
	//  __SYMBOLIC(&tunable_force_dot_files);
	//  __ASSUME(OR((tunable_force_dot_files == 0),(tunable_force_dot_files == 1)));
#endif
#ifdef TUNABLE_LISTEN_IPV6
	  tunable_listen_ipv6 = TUNABLE_LISTEN_IPV6;
#else
	//  __SYMBOLIC(&tunable_listen_ipv6);
	//  __ASSUME(OR((tunable_listen_ipv6 == 0),(tunable_listen_ipv6 == 1)));
#endif
#ifdef TUNABLE_DUAL_LOG_ENABLE
	  tunable_dual_log_enable = TUNABLE_DUAL_LOG_ENABLE;
#else
	  __SYMBOLIC(&tunable_dual_log_enable);
	  __ASSUME(OR((tunable_dual_log_enable == 0),(tunable_dual_log_enable == 1)));
#endif
#ifdef TUNABLE_SYSLOG_ENABLE
	  tunable_syslog_enable = TUNABLE_SYSLOG_ENABLE;
#else
	//  __SYMBOLIC(&tunable_syslog_enable);
	//  __ASSUME(OR((tunable_syslog_enable == 0),(tunable_syslog_enable == 1)));
#endif
#ifdef TUNABLE_BACKGROUND
	  tunable_background = TUNABLE_BACKGROUND;
#else
	//  __SYMBOLIC(&tunable_background);
	//  __ASSUME(OR((tunable_background == 0),(tunable_background == 1)));
#endif
#ifdef TUNABLE_VIRTUAL_USE_LOCAL_PRIVS
	  tunable_virtual_use_local_privs = TUNABLE_VIRTUAL_USE_LOCAL_PRIVS;
#else
	//  __SYMBOLIC(&tunable_virtual_use_local_privs);
	//  __ASSUME(OR((tunable_virtual_use_local_privs == 0),(tunable_virtual_use_local_privs == 1)));
#endif
#ifdef TUNABLE_SESSION_SUPPORT
	  tunable_session_support = TUNABLE_SESSION_SUPPORT;
#else
	//  __SYMBOLIC(&tunable_session_support);
	//  __ASSUME(OR((tunable_session_support == 0),(tunable_session_support == 1)));
#endif
#ifdef TUNABLE_DOWNLOAD_ENABLE
	  tunable_download_enable = TUNABLE_DOWNLOAD_ENABLE;
#else
	//  __SYMBOLIC(&tunable_download_enable);
	//  __ASSUME(OR((tunable_download_enable == 0),(tunable_download_enable == 1)));
#endif
#ifdef TUNABLE_DIRLIST_ENABLE
	  tunable_dirlist_enable = TUNABLE_DIRLIST_ENABLE;
#else
	//  __SYMBOLIC(&tunable_dirlist_enable);
	//  __ASSUME(OR((tunable_dirlist_enable == 0),(tunable_dirlist_enable == 1)));
#endif
#ifdef TUNABLE_CHMOD_ENABLE
	  tunable_chmod_enable = TUNABLE_CHMOD_ENABLE;
#else
	//  __SYMBOLIC(&tunable_chmod_enable);
	//  __ASSUME(OR((tunable_chmod_enable == 0),(tunable_chmod_enable == 1)));
#endif
#ifdef TUNABLE_SECURE_EMAIL_LIST_ENABLE
	  tunable_secure_email_list_enable = TUNABLE_SECURE_EMAIL_LIST_ENABLE;
#else
	//  __SYMBOLIC(&tunable_secure_email_list_enable);
	//  __ASSUME(OR((tunable_secure_email_list_enable == 0),(tunable_secure_email_list_enable == 1)));
#endif
#ifdef TUNABLE_NO_LOG_LOCK
	  tunable_no_log_lock = TUNABLE_NO_LOG_LOCK;
#else
	//  __SYMBOLIC(&tunable_no_log_lock);
	//  __ASSUME(OR((tunable_no_log_lock == 0),(tunable_no_log_lock == 1)));
#endif
#ifdef TUNABLE_SSL_ENABLE
	  tunable_ssl_enable = TUNABLE_SSL_ENABLE;
#else
	  __SYMBOLIC(&tunable_ssl_enable);
	  __ASSUME(OR((tunable_ssl_enable == 0),(tunable_ssl_enable == 1)));
#endif
#ifdef TUNABLE_ALLOW_ANON_SSL
	  tunable_allow_anon_ssl = TUNABLE_ALLOW_ANON_SSL;
#else
	  __SYMBOLIC(&tunable_allow_anon_ssl);
	  __ASSUME(OR((tunable_allow_anon_ssl == 0),(tunable_allow_anon_ssl == 1)));
#endif
#ifdef TUNABLE_FORCE_LOCAL_LOGINS_SSL
	  tunable_force_local_logins_ssl = TUNABLE_FORCE_LOCAL_LOGINS_SSL;
#else
	  __SYMBOLIC(&tunable_force_local_logins_ssl);
	  __ASSUME(OR((tunable_force_local_logins_ssl == 0),(tunable_force_local_logins_ssl == 1)));
#endif
#ifdef TUNABLE_FORCE_LOCAL_DATA_SSL
	  tunable_force_local_data_ssl = TUNABLE_FORCE_LOCAL_DATA_SSL;
#else
	  __SYMBOLIC(&tunable_force_local_data_ssl);
	  __ASSUME(OR((tunable_force_local_data_ssl == 0),(tunable_force_local_data_ssl == 1)));
#endif
#ifdef TUNABLE_SSLV2
	  tunable_sslv2 = TUNABLE_SSLV2;
#else
	  __SYMBOLIC(&tunable_sslv2);
	  __ASSUME(OR((tunable_sslv2 == 0),(tunable_sslv2 == 1)));
#endif
#ifdef TUNABLE_SSLV3
	  tunable_sslv3 = TUNABLE_SSLV3;
#else
	  __SYMBOLIC(&tunable_sslv3);
	  __ASSUME(OR((tunable_sslv3 == 0),(tunable_sslv3 == 1)));
#endif
#ifdef TUNABLE_TLSV1
	  tunable_tlsv1 = TUNABLE_TLSV1;
#else
	  __SYMBOLIC(&tunable_tlsv1);
	  __ASSUME(OR((tunable_tlsv1 == 0),(tunable_tlsv1 == 1)));
#endif
#ifdef TUNABLE_TILDE_USER_ENABLE
	  tunable_tilde_user_enable = TUNABLE_TILDE_USER_ENABLE;
#else
	  __SYMBOLIC(&tunable_tilde_user_enable);
	  __ASSUME(OR((tunable_tilde_user_enable == 0),(tunable_tilde_user_enable == 1)));
#endif
#ifdef TUNABLE_FORCE_ANON_LOGINS_SSL
	  tunable_force_anon_logins_ssl = TUNABLE_FORCE_ANON_LOGINS_SSL;
#else
	  __SYMBOLIC(&tunable_force_anon_logins_ssl);
	  __ASSUME(OR((tunable_force_anon_logins_ssl == 0),(tunable_force_anon_logins_ssl == 1)));
#endif
#ifdef TUNABLE_FORCE_ANON_DATA_SSL
	  tunable_force_anon_data_ssl = TUNABLE_FORCE_ANON_DATA_SSL;
#else
	  __SYMBOLIC(&tunable_force_anon_data_ssl);
	  __ASSUME(OR((tunable_force_anon_data_ssl == 0),(tunable_force_anon_data_ssl == 1)));
#endif
#ifdef TUNABLE_MDTM_WRITE
	  tunable_mdtm_write = TUNABLE_MDTM_WRITE;
#else
	  __SYMBOLIC(&tunable_mdtm_write);
	  __ASSUME(OR((tunable_mdtm_write == 0),(tunable_mdtm_write == 1)));
#endif
#ifdef TUNABLE_LOCK_UPLOAD_FILES
	  tunable_lock_upload_files = TUNABLE_LOCK_UPLOAD_FILES;
#else
	  __SYMBOLIC(&tunable_lock_upload_files);
	  __ASSUME(OR((tunable_lock_upload_files == 0),(tunable_lock_upload_files == 1)));
#endif
#ifdef TUNABLE_PASV_ADDR_RESOLVE
	  tunable_pasv_addr_resolve = TUNABLE_PASV_ADDR_RESOLVE;
#else
	  __SYMBOLIC(&tunable_pasv_addr_resolve);
	  __ASSUME(OR((tunable_pasv_addr_resolve == 0),(tunable_pasv_addr_resolve == 1)));
#endif
#ifdef TUNABLE_DEBUG_SSL
	  tunable_debug_ssl = TUNABLE_DEBUG_SSL;
#else
	  __SYMBOLIC(&tunable_debug_ssl);
	  __ASSUME(OR((tunable_debug_ssl == 0),(tunable_debug_ssl == 1)));
#endif
#ifdef TUNABLE_REQUIRE_CERT
	  tunable_require_cert = TUNABLE_REQUIRE_CERT;
#else
	  __SYMBOLIC(&tunable_require_cert);
	  __ASSUME(OR((tunable_require_cert == 0),(tunable_require_cert == 1)));
#endif
#ifdef TUNABLE_VALIDATE_CERT
	  tunable_validate_cert = TUNABLE_VALIDATE_CERT;
#else
	  __SYMBOLIC(&tunable_validate_cert);
	  __ASSUME(OR((tunable_validate_cert == 0),(tunable_validate_cert == 1)));
#endif
#ifdef TUNABLE_STRICT_SSL_READ_EOF
	  tunable_strict_ssl_read_eof = TUNABLE_STRICT_SSL_READ_EOF;
#else
	  __SYMBOLIC(&tunable_strict_ssl_read_eof);
	  __ASSUME(OR((tunable_strict_ssl_read_eof == 0),(tunable_strict_ssl_read_eof == 1)));
#endif
#ifdef TUNABLE_STRICT_SSL_WRITE_SHUTDOWN
	  tunable_strict_ssl_write_shutdown = TUNABLE_STRICT_SSL_WRITE_SHUTDOWN;
#else
	  __SYMBOLIC(&tunable_strict_ssl_write_shutdown);
	  __ASSUME(OR((tunable_strict_ssl_write_shutdown == 0),(tunable_strict_ssl_write_shutdown == 1)));
#endif
#ifdef TUNABLE_SSL_REQUEST_CERT
	  tunable_ssl_request_cert = TUNABLE_SSL_REQUEST_CERT;
#else
	  __SYMBOLIC(&tunable_ssl_request_cert);
	  __ASSUME(OR((tunable_ssl_request_cert == 0),(tunable_ssl_request_cert == 1)));
#endif
#ifdef TUNABLE_DELETE_FAILED_UPLOADS
	  tunable_delete_failed_uploads = TUNABLE_DELETE_FAILED_UPLOADS;
#else
	  __SYMBOLIC(&tunable_delete_failed_uploads);
	  __ASSUME(OR((tunable_delete_failed_uploads == 0),(tunable_delete_failed_uploads == 1)));
#endif
	
	return;
}
