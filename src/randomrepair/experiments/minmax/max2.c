
/* 2-input max */

int main(void) {
	int input1, input2;
    int max = input1;

    /* assume ((input1 >= 0) && (input2 >= 0)); */
    if (max < input2)
        max = input2;

    /* assert (input1 <= max && input2 <= max); */
    __ASSERT(AND(input1 <= max, input2 <= max));
    return 0;
}
