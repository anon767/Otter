/*  This test leads to stack overflows in many places in STP, because the memset(...) below creates a deeply-nested
    STP array expression of the form: "(...(((a WITH [0] := 0) WITH [1] := 0) WITH [2] := 0) ...) WITH [40000] := 0".

    There is no easy fix for this problem, other than increasing the stack limit.
*/

int const MAX = 10000;  /* smaller size doesn't fail */
void main() {
    int a[MAX];
    int i;
    memset(a,0,MAX*sizeof(int));
    __SYMBOLIC(&i);
    i = a[i];
    i = a[i];  /* segmentation fault */
}
