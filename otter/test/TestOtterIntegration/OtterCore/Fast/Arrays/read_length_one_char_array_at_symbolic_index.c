#pragma no_other_abandoned

int main(void) {
    char a[1];
    int i;
    __SYMBOLIC(&a);
    __SYMBOLIC(&i);
    __ASSUME(i == 0);
    __ASSUME(a[i] != 0);
    __ASSERT(a[i]);
    return 0;
}   
