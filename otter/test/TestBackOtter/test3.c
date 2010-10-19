/*
 * ./otter.pl --dobackotter --noboundsChecking --max-abandoned=2 -Ilibc test/TestBackOtter/test3.c
 *
 * Expected output:
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (n > 2): true
 *          Decision: IF (p): false
 *          Decision: IF (p): true
 *          Decision: IF (p): true
 *          Decision: IF (p): true
 *          Decision: IF (p): true
 *          Decision: f: void (struct s *p )
 *          Decision: IF (n > 0): false
 *          Decision: IF (n > 0): true
 *          Decision: IF (n > 0): true
 *          Decision: IF (n > 0): true
 *
 * -----------------------------------------
 * -----------------------------------------
 * List of Final failing path(s) (length 1)
 * Element: Decision: IF (n > 2): true
 *          Decision: IF (p): false
 *          Decision: IF (p): true
 *          Decision: IF (p): true
 *          Decision: IF (p): true
 *          Decision: f: void (struct s *p )
 *          Decision: IF (n > 0): false
 *          Decision: IF (n > 0): true
 *          Decision: IF (n > 0): true
 *
 * -----------------------------------------
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
