#define MAX 1000
void __FAILURE(void) {}

int output(int n) { /* print n; */ return 1; }

void f(int n) {
    if (!output(n)) __FAILURE();  /* infeasible */
}

void g(int n) {
    if (n) __FAILURE();  /* feasible */
}

void main() {
    int i, j, n; __SYMBOLIC(&n);
    if (n) {
        /* Shorter path to an infeasible failure */
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        f(j);
    } else {
        /* Longer path to a feasible failure */
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        for(i=0;i<MAX;i++) j++;
        g(j);
    }
}

