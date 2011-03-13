// Otter:
// <path>/otter.pl --doexecute --max-abandoned=1 --queue=random-path --random-seed=0 Strawman_Example.c
//
// BackOtter:
// <path>/otter.pl --dobackotter --max-abandoned=1 --bidirectional-search-ratio=-1 --backward-queue=random-path --random-seed=0 Strawman_Example.c
//
int __SYMBOLIC(void *);
void __FAILURE(void) { }

int input() {
    int n; __SYMBOLIC(&n);
    return n;
}

void state10(int c) { if (c == 456) __FAILURE(); }
void state09(int c) { c *= 2; input()?state10(c):state10(c+1); }
void state08(int c) { c *= 2; input()?state09(c):state09(c+1); }
void state07(int c) { c *= 2; input()?state08(c):state08(c+1); }
void state06(int c) { c *= 2; input()?state07(c):state07(c+1); }
void state05(int c) { c *= 2; input()?state06(c):state06(c+1); }
void state04(int c) { c *= 2; input()?state05(c):state05(c+1); }
void state03(int c) { c *= 2; input()?state04(c):state04(c+1); }
void state02(int c) { c *= 2; input()?state03(c):state03(c+1); }
void state01(int c) { c *= 2; input()?state02(c):state02(c+1); }
void state00(int c) { c *= 2; input()?state01(c):state01(c+1); }

int main(void) {
    state00(0);
    return 0;
}

