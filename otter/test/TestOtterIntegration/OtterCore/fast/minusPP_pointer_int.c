#pragma expect_abandoned(failure("Expression involves an integer and a pointer"))
#pragma no_other_results

int main() {
    int n;
    int* a = &n;
    int* b = 0;
    return (a - b);
}

