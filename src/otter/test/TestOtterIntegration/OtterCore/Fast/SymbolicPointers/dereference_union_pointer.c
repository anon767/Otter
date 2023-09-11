/* This tests that pointers to unions with different-sized fields can be dereferenced and the values tested. */
#pragma entry_function("foo")
#pragma expect_abandoned(failure("Dereference"))
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)

typedef union {
    char c;
    int n;
} u;

int foo(u *p) {
    if (p->n < 32) {
        return 1;
    } else {
        return 2;
    }
}

int main(void){
    u x;
    foo(&x);
    return 0;
}
