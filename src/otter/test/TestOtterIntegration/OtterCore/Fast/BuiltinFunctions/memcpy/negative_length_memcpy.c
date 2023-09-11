/* Check that memcpy with negative length raises an out of bounds error. */

#pragma expect_abandoned(out_of_bounds)
#pragma no_other_results

typedef unsigned long size_t;
void * memcpy(void * dest, void * src, size_t len);

int main() {
    int x = 1, y = 2;
    memcpy(&x, &y, -1);
    return 0;
}
