#pragma expect_abandoned(failure("minusPP (p1,p2) not of type (addr,addr)"))
#pragma no_other_results

int main() {
    int n;
    int* a = &n;
    int* b = 0;
    return (a - b);
}

