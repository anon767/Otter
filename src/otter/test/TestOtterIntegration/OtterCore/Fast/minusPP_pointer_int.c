#pragma expect_abandoned(failure("Cannot process minusPP (make_Bytes1,make_Bytes2)"))
#pragma no_other_results

int main() {
    int n;
    int* a = &n;
    int* b = 0;
    return (a - b);
}

