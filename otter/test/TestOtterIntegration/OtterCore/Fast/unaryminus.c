#pragma no_other_abandoned

int main(void) {
    int i, j, k;
    __SYMBOLIC(&i);
    __ASSUME(i == 1);
    __ASSERT(i == 1);
    __ASSERT(-i == -1);
    __ASSERT(-i != -0x01000000);

    __SYMBOLIC(&j);
    __ASSUME(j == 0x01000000);
    __ASSERT(j == 0x01000000);
    __ASSERT(j != 1);
    __ASSERT(-j != -1);
    __ASSERT(-j == -0x01000000);
    
    __SYMBOLIC(&k);
    __ASSUME(k == -1);
    __ASSERT(k == -1);
    __ASSERT(-k == 1);

    __ASSERT(-i == k);
    __ASSERT(i == -k);

    __ASSERT((0-i) == k);
    __ASSERT(i == (0-k));
    return 0;
}
