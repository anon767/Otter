#pragma expect_return()
#pragma no_other_results

extern char* malloc(int size);

void main(void) {
    char* (*f)(int) = &malloc;
    char* s = f(1);
}
