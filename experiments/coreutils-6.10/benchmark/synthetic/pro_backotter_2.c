#define MAX 1000
void __FAILURE(void) {}

int output(int n) { /* print n; */ return 1; }

void f(int n) {
    if (!output(n)) __FAILURE();  /* infeasible */
}

void g(int n) {
    if (output(n)) __FAILURE();  /* feasible */
}

void main() {
    int i, j, n; __SYMBOLIC(&n);
    switch (n) {
        case 1:
            /* Shorter path to an infeasible failure */
            for(i=0;i<MAX;i++) { j++; j++; j++; j++; }
            f(j);
            break;
        case 2:
            /* Shorter path to an infeasible failure */
            for(i=0;i<MAX;i++) { j++; j++; j++; j++; }
            f(j);
            break;
        case 3:
            /* Shorter path to an infeasible failure */
            for(i=0;i<MAX;i++) { j++; j++; j++; j++; }
            f(j);
            break;
        case 4:
            /* Shorter path to an infeasible failure */
            for(i=0;i<MAX;i++) { j++; j++; j++; j++; }
            f(j);
            break;
        case 5:
            /* Longer path to a feasible failure */
            for(i=0;i<MAX;i++) { j++; j++; j++; j++; j++; }
            g(j);
            break;
    }
}

