#pragma entry_function("foo")
#pragma time_limit(1)
#pragma expect_return(__return_code__ == 1) /* for the assignment to the non-null leaf node */
#pragma no_other_abandoned

/* This tests that malloc'ed structs, which contain pointers, that are reached via a pointer to a field are initialized
   correctly. Here, x points to y->f, where y is a struct s. When x is dereferenced in foo, a fresh struct s
   should be initialized (as an alias of y), not a fresh int (i.e., the type pointed to by x), and x initialized to
   point to field f of that struct.
*/

int *x;
struct s {
    int *a;
    int f;
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
    x = &y->f;
    return foo();
}
