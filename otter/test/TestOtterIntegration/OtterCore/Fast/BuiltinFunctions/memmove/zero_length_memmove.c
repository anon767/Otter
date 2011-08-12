/* Check that memmove works with zero length. */

#pragma expect_return()
#pragma no_other_results

typedef unsigned long size_t;
void * memmove(void * dest, void * src, size_t len);

int main() {
    int x = 1, y = 2;
    memmove(&x, &y, 0);
    __ASSERT(x == 1);
    __ASSERT(y == 2);
    return 0;
}
