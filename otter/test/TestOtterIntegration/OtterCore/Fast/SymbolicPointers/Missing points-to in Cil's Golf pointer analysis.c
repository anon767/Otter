/*  This test reveals an odd bug in Cil's Golf pointer analysis, which for some reason does not pick up that y points
    to the malloc()'ed memory in main() (y == *w == z == malloc). Because x points to y, all three branches in foo()
    should be feasible, but the third one is missed if Golf is used.
*/

#pragma entry_function("foo")
#pragma expect_return(__return_code__ == 0)
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)
#pragma no_other_results

typedef unsigned long size_t;
void * malloc(size_t size);

typedef struct dll {
    struct dll *next;
} dll;

int foo(dll **x) {
    if (!x) {
        return 0;
    } else if (*x == 0) {
        return 1;
    } else {
        return 2; /* this line doesn't get executed if Golf is used */
    }
    (*x)->next = 0; /* the bug disappears if we omit this line */
}

void bar(dll **w) {
    dll *y = *w;
    foo(&y);
}

int main(void) {
    dll *z = malloc(sizeof(dll));
    bar(&z);
    return 0;
}

