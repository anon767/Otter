
/* khooyp: from A. Groce. "Error Explanation with Distance Metrics". (2004) */

int main(void) {
    int input1, input2, input3; //symbolic

    int least = input1;
    int most = input1;

	__ASSUME(input1>=0,input2>=0,input3>=0);

    if (most < input2)
        most = input2;

    if (most < input3)
        most = input3;

    if (least > input2)
        most = input2; /* ERROR: should be least = input2 */
    	//least = input3; /* also an error, but passes the assertion least<=most */
        //least = input2; 

    if (least > input3)
        least = input3;

    /* assert (least <= most); */
	//__ASSERT(least<=most); /* incomplete assertion */
    __ASSERT(least<=input1,least<=input2,least<=input3,most>=input1,most>=input2,most>=input3);

    return 0;
}
