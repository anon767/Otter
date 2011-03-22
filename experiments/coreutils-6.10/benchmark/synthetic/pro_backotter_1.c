// Otter:
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=closest-to-targets --random-seed=0 pro_backotter_1.c --printLittle
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=random-path --random-seed=0 pro_backotter_1.c --printLittle
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=KLEE --random-seed=0 pro_backotter_1.c --printLittle
//
// BackOtter:
// <path>/otter.pl --dobackotter --max-abandoned=1 --bidirectional-search-ratio=-1 --backward-queue=random-path --random-seed=0 pro_backotter_1.c --backward-function-rank=closest-to-entry --printLittle
//
#define MAX 8
void __FAILURE(void) {}

void f(int n) { 
    int i, c = 0, input[MAX];
    __SYMBOLIC(&input);
    for(i=0;i<MAX;i++) {
        if (input[i]) c++;
    }
    /* rarely satisfied condition */
    if (c == 3 && n == 4) __FAILURE(); 
}

void main() {
    int i, c = 0, input[MAX];
    __SYMBOLIC(&input);
    for(i=0;i<MAX;i++) {
        if (input[i]) c++;
    }
    f(c);
}

