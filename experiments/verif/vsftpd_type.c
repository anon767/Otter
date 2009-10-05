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

 int old_is_ascii;
 char* c;

 // init state
 init_state();

 old_is_ascii = sess.is_ascii;
 c = sess.ftp_arg_str.PRIVATE_HANDS_OFF_p_buf;

 __CURRENT_STATE(0);
 handle_type(&sess);
 __CURRENT_STATE(1);

 if(c[0]=='A' || c[0]=='a'){
	__COMMENT("A");
 	__ASSERT(sess.is_ascii!=0);
 }
 else if(c[0]=='I' || c[0]=='i'){
	__COMMENT("I");
 	__ASSERT(sess.is_ascii==0);
 }
 else{
	__COMMENT("N");
 	__ASSERT(sess.is_ascii==old_is_ascii);
 }

 __COMPARE_STATE(0,1);

 return 0;
}
