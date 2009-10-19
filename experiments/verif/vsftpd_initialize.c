#define main vsftpd_main
//#include "vsftpd_comb_special.c"
#include "vsftpd_comb.c"
#undef main

// In vsftpd_comb_special.c, we:
// disabled vsnprintf
// added __ctype_b_loc

// client session
struct vsf_session sess;


// Just a workaround, not mean to be the ultimate solution
void init__ctypes();

// Create a new (empty) sym_file_stream_t. If fd >= 0, attach the
// stream at file descriptor fd.
sym_file_stream_t *newStream(int fd) {
	sym_file_stream_t *stream = malloc(sizeof(sym_file_stream_t));
	stream->offset = 0;
	stream->sym_file = malloc(sizeof(sym_file_t));
	stream->sym_file->contents = 0;
	stream->sym_file->stat.st_size = 0;
	if (fd >= 0) {
		stream->fd = fd;
		IOSIM_fd[fd] = stream;
	}
	return stream;
}

void initstr(struct mystr* s){
#ifdef TEST_CONCRETE_STRING
 s->PRIVATE_HANDS_OFF_alloc_bytes = 0;
 s->PRIVATE_HANDS_OFF_p_buf = 0; 
 s->PRIVATE_HANDS_OFF_len = 0;
#else
 s->PRIVATE_HANDS_OFF_alloc_bytes = __SYMBOLIC();
 __ASSUME(s->PRIVATE_HANDS_OFF_alloc_bytes>0);
 s->PRIVATE_HANDS_OFF_p_buf = malloc(10); // arbitrary; right now memory is unbounded.
 s->PRIVATE_HANDS_OFF_len = __SYMBOLIC();
#endif
}

char* symbolic_string(int size){
	char* s = malloc(size+1);
	for (int i=0;i<size;i++){
		s[i] = __SYMBOLIC();
		__ASSUME(s[i]!='\0');
	}
	s[size] = '\0';
	return s;
}

int init_state(){
 __SYMBOLIC_STATE();

 // Oh yeah, __SYMBOLIC_STATE() wipes out everything, INCLUDING the ctype table!!
 init__ctypes();
 // recover some constants
 IOSIM_num_file = 0;
 IOSIM_num_fd = 3;

 // a str is set up so that if alloc_bytes>0, p_buf!=0
 initstr(&sess.ftp_arg_str);
 initstr(&sess.user_str);
 initstr(&sess.remote_ip_str);
 initstr(&sess.log_str);

 sess.vsftpd_log_fd = -1;
 tunable_syslog_enable = 0; // important
 tunable_one_process_model = 1; // may require

 // IDEA: have to dereference symbolic values if not set these
 sess.ssl_consumer_fd = 0;
 sess.ssl_slave_active = 0;

 // These are static strings
 initstr(&s_the_str);
 initstr(&s_the_str___0);
 initstr(&s_write_buf_str);
 initstr(&s_text_mangle_str);
 initstr(&s_log_str);
 initstr(&s_src_str);
 initstr(&s_rhs_str);
 initstr(&s_cwd_buf_mangle_str);
 initstr(&s_pwd_res_str);
 initstr(&s_pasv_res_str);
 initstr(&s_mark_str);
 initstr(&s_option_str);
 initstr(&s_filter_str);
 initstr(&s_dir_name_str);
 initstr(&s_filename);
 initstr(&s_mkd_res);
 initstr(&s_tmp_str);
 initstr(&s_rest_str);
 initstr(&s_tmp_str___0);
 initstr(&s_tmp_str___1);
 initstr(&s_size_res_str);
 initstr(&s_site_args_str);
 initstr(&s_chmod_file_str);
 initstr(&s_umask_resp_str);
 initstr(&s_filename_str);
 initstr(&s_mdtm_res_str);
 initstr(&s_part1_str);
 initstr(&s_part2_str);
 initstr(&s_rhs_str___0);
 initstr(&s_user_str);
 initstr(&s_next_filename_str);
 initstr(&s_next_path_and_filename_str);
 initstr(&s_final_file_str);
 initstr(&s_temp_str);
 initstr(&s_temp_str___0);
 initstr(&s_match_needed_str);
 initstr(&s_tmp_str___2);
 initstr(&s_log_str___0);
 initstr(&s_tmp_str___3);
 initstr(&s_lhs_chunk_str);
 initstr(&s_rhs_chunk_str);
 initstr(&s_curr_line_str);
 initstr(&s_null_str);
 initstr(&s_ret);
 initstr(&s_rhs_ret);
 initstr(&s_lhs_str);
 initstr(&s_rhs_str___1);
 initstr(&s_tmp_str___4);
 initstr(&s_rhs_sep_str);
 initstr(&s_lhs_str___0);
 initstr(&s_rhs_str___2);
 initstr(&s_access_str);
 initstr(&s_access_str___0);
 initstr(&s_proctitle_prefix_str);
 initstr(&s_pword_str);

 // these are static char* :
 // static struct vsf_sysutil_statbuf * 
 s_p_statbuf  =0;
 s_p_dirstat  =0;
 s_p_statbuf___0  =0;
 p_statbuf  =0;
 s_p_statbuf___1  =0;
 s_p_statbuf___2  =0;
 s_p_statbuf___3  =0;
 s_p_statbuf___4  =0;
 s_p_statbuf___5  =0;
 s_p_statbuf___6  =0;
 // static char *
 nextchar  =0;
 posixly_correct  =0;
 holder  =0;
 p_readbuf  =0;
 p_asciibuf  =0;
 p_recvbuf  =0;
 p_getcwd_buf  =0;
 p_readlink_buf  =0;
 p_recvbuf___0  =0;

 // Other static values that are zero until they are initialized
 s_page_size = 0;

 memcpy(lcdigits,"0123456789abcdef",sizeof(lcdigits));
 memcpy(ucdigits,"0123456789ABCDEF",sizeof(ucdigits));

 // no signals
 for(int i=0;i<65;i++)
	 s_sig_details[i].pending = 0;

 // invariant?
 s_io_handler = 0;
 s_exit_func = 0;
 
 // set up stdout
 newStream(0);

 // *some* symbolic string input
 str_alloc_text(&sess.user_str,symbolic_string(1));
 str_alloc_text(&sess.ftp_arg_str,symbolic_string(1));
 str_alloc_text(&sess.remote_ip_str,symbolic_string(1));

 return 0;
}

void init__ctypes(){
	unsigned char const   my__ctypes[257]  = 
	  {      (unsigned char const   )0,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )144,      (unsigned char const   )144,      (unsigned char const   )144, 
	        (unsigned char const   )144,      (unsigned char const   )144,      (unsigned char const   )144,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )48,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )44,      (unsigned char const   )44,      (unsigned char const   )44, 
	        (unsigned char const   )44,      (unsigned char const   )44,      (unsigned char const   )44,      (unsigned char const   )44, 
	        (unsigned char const   )44,      (unsigned char const   )44,      (unsigned char const   )44,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )41,      (unsigned char const   )41, 
	        (unsigned char const   )41,      (unsigned char const   )41,      (unsigned char const   )41,      (unsigned char const   )41, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )42,      (unsigned char const   )42, 
	        (unsigned char const   )42,      (unsigned char const   )42,      (unsigned char const   )42,      (unsigned char const   )42, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128,      (unsigned char const   )128, 
	        (unsigned char const   )128,      (unsigned char const   )48,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96,      (unsigned char const   )96, 
	        (unsigned char const   )96,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )96,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33,      (unsigned char const   )33, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )96,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34,      (unsigned char const   )34, 
	        (unsigned char const   )34};
	for(int i=0;i<257;i++)
		__ctypes[i] = my__ctypes[i];
}

