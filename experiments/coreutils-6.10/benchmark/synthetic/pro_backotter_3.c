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

void f(int m) {
    int g = 0;

    while(1) {
        if(g > 10 && m > 10) failure();
        else g++;
    }
}

void main() {
    int n, i;
    symbolic(&n, sizeof(n), "n");

    for (i=0;i<1000;i++) 
        if (n == i) f(i);
}
