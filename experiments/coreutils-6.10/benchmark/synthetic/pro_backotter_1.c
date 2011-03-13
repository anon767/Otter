// Otter (~12 secs)
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=random-path --random-seed=0 pro_backotter_1.c --printLittle
//
// BackOtter (~2 secs)
// <path>/otter.pl --dobackotter --max-abandoned=1 --bidirectional-search-ratio=-1 --backward-queue=random-path --random-seed=0 pro_backotter_1.c --backward-function-rank=closest-to-entry --printLittle
//
void __FAILURE(void) {}

void f(int n) { 
    int j, c = 0;
    for(j=0;j<9;j++) {
        int m;
        __SYMBOLIC(&m);
        if (m) c++;
    }
    if (c == 0 && n == 5/* rarely satisfied condition */) __FAILURE(); 
}

void main() {
    int n, i; 
    __SYMBOLIC(&n);
    /* Lots of branches induced by i */
    for (i=0;i<50;i++)
        if(n==i) break;
    f(n);
}

