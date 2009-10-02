// TODO
// Find the loop invariant at the beginning of the event driven loop/handler functions
// by (concrete) runs and summarize the program state at that point.
//
// Possible bugs in Otter:
// 1. to assert that addr returned by malloc != 0
// 2. global init (gone)
//
#define main vsftpd_main
#include "vsftpd_comb_special.c"
#undef main

// Just a workaround, not mean to be the ultimate solution
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

char* symbolic_string(int size){
	char* s = malloc(size+1);
	for (int i=0;i<size;i++){
		s[i] = __SYMBOLIC();
		__ASSUME(s[i]!='\0');
	}
	s[size] = '\0';
	return s;
}

void initstr(struct mystr* s){
//  unsigned bufsize = __SYMBOLIC(), len = __SYMBOLIC();
//  __ASSUME(len < bufsize);
//  s->PRIVATE_HANDS_OFF_alloc_bytes = bufsize;
//  s->PRIVATE_HANDS_OFF_p_buf = symbolic_string(bufsize);
//  s->PRIVATE_HANDS_OFF_len = len;
  memset(s,0,sizeof(*s));
}

struct vsf_session sess;

int main(){
 char* c;
 __SYMBOLIC();
 // ANY-state
 __SYMBOLIC_STATE();

 // Oh yeah, it wipes out everything, INCLUDING the ctype table!!
 init__ctypes();

 memcpy(lcdigits,"0123456789abcdef",sizeof(lcdigits));
 memcpy(ucdigits,"0123456789ABCDEF",sizeof(ucdigits));

 // a str is set up so that if alloc_bytes>0, p_buf!=0
 initstr(&sess.ftp_arg_str);
 initstr(&sess.user_str);
 initstr(&sess.remote_ip_str);
 // TODO: restrict ANY-state so that an inconsistent str does not exist
 // IDEA: report errors to the user, and let the user refine the ANY-state

 sess.vsftpd_log_fd = -1;
 tunable_syslog_enable = 0; // important

 // IDEA: have to dereference symbolic values if not set these
 sess.ssl_consumer_fd = 0;
 sess.ssl_slave_active = 0;

 // hacks: changed the below static strings to non-static!
 initstr(&s_the_str___0);
 initstr(&s_text_mangle_str);
 initstr(&s_lhs_chunk_str);
 initstr(&s_rhs_chunk_str);
 initstr(&s_write_buf_str);
 initstr(&s_log_str___0);

 initstr(&s_cwd_buf_mangle_str);
 initstr(&s_pwd_res_str);
 p_getcwd_buf = 0;
 s_page_size = 0;

 // hack: changed the below static array to non-static!
 for(int i=0;i<65;i++)
	 s_sig_details[i].pending = 0;

 // invariant?
 s_io_handler = 0;
 s_exit_func = 0;
 
 // disabled vsnprintf
 // added __ctype_b_loc

 // set up the command fd
 newStream(0);

 // *some* symbolic string input
// c = symbolic_string(1);
//
// str_alloc_text(&sess.ftp_arg_str,c);
// str_alloc_text(&sess.user_str,symbolic_string(1));
// str_alloc_text(&sess.remote_ip_str,symbolic_string(1));

 __CURRENT_STATE(0);
 handle_pwd(&sess);

 __ASSERT_EQUAL_STATE(0);

 return 0;
}
