#pragma entry_function("foo")
#pragma expect_return(__return_code__ == 0)
#pragma expect_return(__return_code__ == 1)
#pragma expect_return(__return_code__ == 2)
#pragma expect_return(__return_code__ == 3)
#pragma no_other_results

/* the analysis initializes x (as well as y) to one of 3 states: NULL, &z or a fresh block */

int foo(int * x, int * y) {
    if (!x) {
        return 0;
    } else if (!y) {
        return 1;
    } else if (x == y) {
        return 2;
    } else if (x != y) {
        return 3;
    } else {
        return 4;
    }
}

void main(void) {
    int z;
    foo(&z, &z);
}
