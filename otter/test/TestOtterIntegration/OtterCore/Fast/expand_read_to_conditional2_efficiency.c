#pragma time_limit(3)
#pragma expect_return()
#pragma no_other_results

void main(void) {
    int* a[256];
    int p = 1;
    a[10] = &p;
    int c; __SYMBOLIC(&c); __ASSUME(c==10);
    int* q = a[c];
    __EVAL(q);  // Read(Bytearray(\#{40}BB(...)\B\B\B\#{980}), ...)
    __EVAL(*q); // This can be expensive if the above Read structure is expanded to a conditional tree, one guard per index.
}
