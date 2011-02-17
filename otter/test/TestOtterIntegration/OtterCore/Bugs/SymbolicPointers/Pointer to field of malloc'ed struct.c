#pragma expect_return()
#pragma expect_return()
#pragma no_other_results

/* This tests that malloc'ed structs reached via a pointer to a field are initialized correctly. */

int *x;
struct s {
    int *a;
    int b;
};
void foo(void) {
    if (x) {
        *x = 1;
    }
}
int main(void) {
    struct s * y = (struct s *)malloc(sizeof(struct s));
    x = &y->a;
    foo();
    return 0;
}
