int const MAX = 10000;  /* smaller size doesn't fail */
void main() {
    int a[MAX];
    int i;
    memset(a,0,MAX*sizeof(int));
    __SYMBOLIC(&i);
    i = a[i];
    i = a[i];  /* segmentation fault */
}
