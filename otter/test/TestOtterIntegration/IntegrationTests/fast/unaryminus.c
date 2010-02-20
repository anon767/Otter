int main(void) {
    int i,j;
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
    return 0;
}
