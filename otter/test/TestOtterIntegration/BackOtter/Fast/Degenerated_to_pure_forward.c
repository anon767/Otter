#pragma expect_abandoned(target_reached, x > -5)
#pragma expect_abandoned(target_reached, x <= -5)
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma expect_return()
#pragma no_other_abandoned
#pragma no_other_return
#pragma no_other_exit
#pragma bidirectional_search_ratio("1.0")

int x;

void __FAILURE(void) { }

void nothing(void) {
    // This function is created to make sure that the cfg-pruning
    // works properly with function calls
}

void g(int y) {
    nothing();
    if (y > 0) {
        if (y < 10) {
            __FAILURE();
        }
    }
}

void f(int z) {
    if (z > -5) {
        g(z);
    } else {
        g(-z);
    }
}

void main(void) {
    __SYMBOLIC(&x);
    f(x);
}
