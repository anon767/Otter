/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=(any number) test/TestBackOtter/bug1.c
 */
#include "otter.h"

struct doubly_linkedlist {
    struct doubly_linkedlist* next;
};
#define DL  struct doubly_linkedlist

void f(DL** x) {
    // BUG: When (*x==0) is evaluated, a conditional exception for (x==0) is
    // raised. But for (x!=0), (*x==0) is evaluated to True. Why?
    if (*x == 0) {
        __ASSERT(0);
    } else {
        // The bug disappears if we comment out the line below, or
        // simplify it to *x = 0.
        (*x)->next = 0;
    }
}

// The bug disappears if we omit this middle function
void g(DL** x) {
    DL* y = *x;
    f(&y);
}

void main() {
    DL* x = malloc(sizeof(DL));
    g(&x);
}
