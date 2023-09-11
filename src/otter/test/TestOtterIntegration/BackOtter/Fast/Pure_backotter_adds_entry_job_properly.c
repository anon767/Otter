#pragma max_abandoned(1)
#pragma time_limit(1)
#pragma expect_abandoned(target_reached)
#pragma bidirectional_search_ratio("-1.0")  // Pure BackOtter

void __FAILURE(void) { }

void f(int x) {
    while (--x);
    __FAILURE();
}
void main(void) {
    int x;
    __SYMBOLIC(&x);
    f(x);
}
