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

#define TEST_CONCRETE_STRING
#include "vsftpd_initialize.c"

int main(){

 init_state();

 // made arg concrete for now
 // problem: it first converts ascii to long, and last converts back to ascii
 //str_alloc_text(&sess.ftp_arg_str,"1");

 __CURRENT_STATE(0);
 handle_rest(&sess);
 __CURRENT_STATE(1);

 __COMPARE_STATE(0,1);

 return 0;
}
