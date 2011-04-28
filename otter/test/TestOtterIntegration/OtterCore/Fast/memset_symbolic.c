/* This tests makes sure that a C implementation of memset is invoked
 * when it's given a symbolic length. */
#pragma expect_abandoned(out_of_bounds)
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma no_other_results

char* memset(char *b, int c, int len) {
    int i;
    for (i=0;i<len;i++) 
        b[i] = c;
    return b;
}

void main() {
    char a[5];
    int n; __SYMBOLIC(&n);
    memset(a,0,n);
}
