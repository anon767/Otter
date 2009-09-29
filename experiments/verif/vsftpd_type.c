// TODO
// Find the loop invariant at the beginning of the event driven loop/handler functions
// by (concrete) runs and summarize the program state at that point.
//
// Possible bugs in Otter:
// 1. to assert that addr returned by malloc != 0
// 2. global init
//
#define main vsftpd_main
#include "vsftpd_comb_special.c"
#undef main

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
 s->PRIVATE_HANDS_OFF_alloc_bytes = 0;
 s->PRIVATE_HANDS_OFF_p_buf = 0;
 s->PRIVATE_HANDS_OFF_len = 0;
}

int main(){
 struct vsf_session sess;
 char c[2];
 int old_is_ascii;

 // ANY-state
 __SYMBOLIC_STATE();

 __SYMBOLIC(&sess);
 // a str is set up so that if alloc_bytes>0, p_buf!=0
 initstr(&sess.ftp_arg_str);
 initstr(&sess.user_str);
 initstr(&sess.remote_ip_str);
 // TODO: restrict ANY-state so that an inconsistent str does not exist
 // IDEA: report errors to the user, and let the user refine the ANY-state

 sess.vsftpd_log_fd = 0;

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

 // hack: changed the below static array to non-static!
 for(int i=0;i<65;i++)
	 s_sig_details[i].pending = 0;

 // invariant?
 s_io_handler_running = 1;
 s_exit_func = 0;
 
 // disabled vsnprintf
 // added __ctype_b_loc

 // set up stdout
 newStream(0);

 // *some* symbolic string input
 c[0] = __SYMBOLIC();
 c[1] = '\0';

 str_alloc_text(&sess.ftp_arg_str,c);
 str_alloc_text(&sess.user_str,c);
 str_alloc_text(&sess.remote_ip_str,c);

 old_is_ascii = sess.is_ascii;

 handle_type(&sess);

 if(c[0]=='A' || c[0]=='a')
 	__ASSERT(sess.is_ascii!=0);
 else if(c[0]=='I' || c[0]=='i')
 	__ASSERT(sess.is_ascii==0);
 else
 	__ASSERT(sess.is_ascii==old_is_ascii);

 return 0;
}
