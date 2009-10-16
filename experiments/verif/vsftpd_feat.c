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

 __CURRENT_STATE(0);
 handle_feat(&sess);
 __CURRENT_STATE(1);

 __COMPARE_STATE(0,1);

 return 0;
}
