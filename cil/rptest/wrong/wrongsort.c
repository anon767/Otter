
/* khooyp: a sorry attempt at sorting */

#define INPUT_SIZE 3

int main(void) {
	int input[INPUT_SIZE];
	
	for (int i = 1; i < INPUT_SIZE; i++) { /* ERROR: missing the inner loop */
		if (input[i-1] > input[i]) {
			int tmp = input[i];
			input[i] = input[i-1];
			input[i-1] = tmp;
		}
	}

    /* assert (input[0] <= input[1] <= input[2]); */
	__ASSERT(AND(input[0]<=input[1],input[1]<=input[2]));
    return 0;
}
