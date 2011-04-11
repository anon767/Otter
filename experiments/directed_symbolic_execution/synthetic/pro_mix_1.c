#ifdef KLEE
  #include <assert.h>
  #define symbolic(V,S,N)   klee_make_symbolic(V,S,N)
  #define assume(E)         klee_assume(E)
#else
  #ifdef CIL
    void __FAILURE(void) {}
    #define symbolic(V,S,N)   __SYMBOLIC(V)
    #define assert(a)         if (a); else __FAILURE()
    #define assume(E)         __ASSUME(E)
  #else
    #warning "Either run with CIL or KLEE"
  #endif
#endif

int work() { return 0; }

void f(int n) {
    int g = 0;

    while(1) {
        if(g > 1000 && n == 109) assert(0);
        else g++;
    }
}

void g(int n) {
    int i;
    for (i=0;i<1000;i++)
        if (n == i) f(i);
}

void main() {
    int i, n;
    symbolic(&n, sizeof(n), "n");
    work();
    work();
    work();
    if (n < 100) return;
    g(n);
}
