// The bug is fixed in r12972; this test shouldn't exhibit an infinite recursion.

// Before r12972, s[0]+1 triggers Operator.opPI's Bytes_Read pattern, hence expand_read_to_conditional is called with a being Bytes_Symbolic. 
// But then this gives back a read on Bytes_Symbolic, which is applied with opPI again. This creates an infinite recursion.

void main(void) {
    int n; __SYMBOLIC(&n); 
    int **s = malloc(n);
    __EVAL(s[0]+1);
}

