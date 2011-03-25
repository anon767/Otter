void __FAILURE(void) {}

#ifdef KLEE
  #include <assert.h>
  #define symbolic(V,S,N)   klee_make_symbolic(V,S,N)
  #define failure()         assert(0)
#else
  #ifdef CIL
    #define symbolic(V,S,N)   __SYMBOLIC(V)
    #define failure()         __FAILURE()
  #else
    #warning "Either run with CIL or KLEE"
  #endif
#endif

void f(int n) {
    int g = 0;

    while(1) {
        if(g > 1000 && n == 0) failure();
        else g++;
    }
}

void main() {
    int n, i;
    symbolic(&n, sizeof(n), "n");

    for (i=1;i<1000;i++)
        if (n == i) break;
    switch (i) {
        case 1: case 2: case 3: 
        case 4: case 5: case 6: 
        case 7: case 8: case 9: 
            f(1); break;
        default: f(0);
    }
}
