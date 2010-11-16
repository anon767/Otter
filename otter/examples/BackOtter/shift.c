int input() {
    int n; __SYMBOLIC(&n);
    return n;
}
void state20(int c) { if (c == 123456) __FAILURE(); }
void state19(int c) { c *= 2; input()?state20(c):state20(c+1); }
void state18(int c) { c *= 2; input()?state19(c):state19(c+1); }
void state17(int c) { c *= 2; input()?state18(c):state18(c+1); }
void state16(int c) { c *= 2; input()?state17(c):state17(c+1); }
void state15(int c) { c *= 2; input()?state16(c):state16(c+1); }
void state14(int c) { c *= 2; input()?state15(c):state15(c+1); }
void state13(int c) { c *= 2; input()?state14(c):state14(c+1); }
void state12(int c) { c *= 2; input()?state13(c):state13(c+1); }
void state11(int c) { c *= 2; input()?state12(c):state12(c+1); }
void state10(int c) { c *= 2; input()?state11(c):state11(c+1); }
void state09(int c) { c *= 2; input()?state10(c):state10(c+1); }
void state08(int c) { c *= 2; input()?state09(c):state09(c+1); }
void state07(int c) { c *= 2; input()?state08(c):state08(c+1); }
void state06(int c) { c *= 2; input()?state07(c):state07(c+1); }
void state05(int c) { c *= 2; input()?state06(c):state06(c+1); }
void state04(int c) { c *= 2; input()?state05(c):state05(c+1); }
void state03(int c) { c *= 2; input()?state04(c):state04(c+1); }
void state02(int c) { c *= 2; input()?state03(c):state03(c+1); }
void state01(int c) { c *= 2; input()?state02(c):state02(c+1); }
void state00(int c) { c *= 2; input()?state01(c):state01(c+1); }

int main(void) {
    int n; __SYMBOLIC(&n);
    state00(n);
    return 0;
}

