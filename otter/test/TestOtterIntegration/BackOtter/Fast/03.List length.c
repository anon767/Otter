/*
 * ./otter.pl --dobackotter 03.List length.c
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

#pragma expect_abandoned(failure_reached, n == 3)
#pragma expect_abandoned(failure_reached, n == 2)
#pragma expect_return(n == 1)
#pragma expect_return(n <= 0)
#pragma no_other_abandoned
#pragma no_other_return
#pragma no_other_exit

int n;

void __FAILURE(void) { }

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
    struct s* head = malloc(sizeof(struct s));
    struct s* cur = head;

    __SYMBOLIC(&n);
    __ASSUME(n<4);

    int i = n;
    while(i>0) {
        cur->next = malloc(sizeof(struct s));
        cur = cur->next;
        i--;
    }
    cur->next = 0;
    f(head);
}
