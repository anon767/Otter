// TODO
// Find the loop invariant at the beginning of the event driven loop/handler functions
// by (concrete) runs and summarize the program state at that point.
//
// Possible bugs in Otter:
// 1. to assert that addr returned by malloc != 0
// 2. global init (gone)
//
#define TEST_CONCRETE_STRING
#include "vsftpd_initialize.c"

int main(){

 // init state
 init_state();
 
 // IDEALLY: cover "UTF8 ON"
 // run with --mergePaths
 
 // On every call to PASV/PORT, these fields are cleared at the beginning.
 // Therefore they are never active at the same time.
 // IF they were both active at the same time, the function call
 // data_transfer_checks_ok(p_sess) at the beginning of handle_retr will loop forever!
 sess.p_port_sockaddr = 0;
 sess.pasv_listen_fd = -1;

 // suppose we have PASV:
 sess.pasv_listen_fd = 1;

 initstr(&sess.home_str);
 str_alloc_text(&sess.home_str,symbolic_string(1));
 strcpy(workingDir,"/");
 strcpy(workingDir+1,symbolic_string(1));
 tunable_deny_file = strdup("tunable_deny_file");

 // retr file "tmp"
 char file[10];
 strcpy(file,"tmp");
 //strcpy(file,"/");
 //strcpy(file+1,symbolic_string(1));
 IOSIM_addfile(file,0);
 str_alloc_text(&sess.ftp_arg_str,file);

 //__CURRENT_STATE(0);
 handle_retr(&sess);
 //__CURRENT_STATE(1);

 //__COMPARE_STATE(0,1);

 return 0;
}
