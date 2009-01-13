
/* 3-input max */

int main(void) {
	int input1, input2, input3;
    int max = input1;

    /* assume ((input1 >= 0) && (input2 >= 0) && (input3 >= 0)); */

    if (max < input2)
        max = input2;
    
    if (max < input3)
    	max = input2; /* ERROR: should be max = input3 */

    /* assert (input1 <= max && input2 <= max  && input3 <= max); */
    __ASSERT(input1 <= max, input2 <= max, input3 <= max);
    return 0;
}
