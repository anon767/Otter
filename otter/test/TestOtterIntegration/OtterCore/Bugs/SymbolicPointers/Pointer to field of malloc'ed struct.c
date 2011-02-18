#pragma entry_function("foo")
#pragma time_limit(1)
#pragma expect_return(__return_code__ == 1) /* for the assignment to the non-null leaf node */
#pragma no_other_abandoned

/* This tests that malloc'ed structs reached via a pointer to a field are initialized correctly. */

int *x;
struct s {
    int *a;
    int b;
};
int foo(void) {
    if (x) {
        *x = 1;
        return 1;
    }
    return 0;
}
int main(void) {
    struct s * y = malloc(sizeof(struct s));
    x = &y->a;
    return foo();
}
