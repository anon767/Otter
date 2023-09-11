#define TEST_CONCRETE_STRING
#include "vsftpd_initialize.c"

int main(){

 // init state
 init_state();

 // Some data
 // len	&	running time (s) on Elnatan's laptop	&	\# paths
 // Not merging paths
 // 0	&	1			&	6
 // 1	&	3			&	30
 // 2	&	5			&	126
 // 3	&	18			&	510
 // 4	&	72			&	2046
 // 5	&	453			&	8190
 //
 // Merging paths
 // 0	&	3			&	2
 // 1	&	24			&	6

 // Make current working directory at most MAX_DIR_LEN characters long.
#define MAX_DIR_LEN 4
 workingDir[MAX_DIR_LEN]=0;

 // __CURRENT_STATE(0);
 handle_pwd(&sess);

 // __ASSERT_EQUAL_STATE(0);

 return 0;
}
