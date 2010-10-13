/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=(any) -Ilibc test/TestBackOtter/test3.c
 */
#include "otter.h"

struct s {
    struct s* next;
    int x;
};

void f(struct s* p) {
    int n = 0;
    while (p) {
        n++;
        p = p->next;
    }
    if (n > 2) {
        __FAILURE();
    }
}

void main() {
    int n;
    struct s* head = malloc(sizeof(struct s));
    struct s* cur = head;
    __SYMBOLIC(&n);
    while(n>0) {
        cur->next = malloc(sizeof(struct s));
        cur = cur->next;
        n--;
    }
    cur->next = 0;
    f(head);
}
