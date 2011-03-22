void __FAILURE(void) {}

int g;

void f(int m) {
    if (g>10 && m<10) __FAILURE();
}

void main() {
    while(1) {
        int m; __SYMBOLIC(&m);
        if(m) f(m);
        else {
            g++;
            g++;
        }
    }
}
