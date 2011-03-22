void __FAILURE(void) {}

void f(int m) {
    int g = 0;

    while(1) {
        if(g > 10 && m > 10) __FAILURE();
        else { g++; g++; }
    }
}

void main() {
    int n, i;
    __SYMBOLIC(&n);

    for (i=0;i<100;i++) 
        if (n == i) f(i);
}
