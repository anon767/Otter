#pragma expect_abandoned(failure("Error: write to a const"))
#pragma no_other_results

const int x = 0;
int main() {
    x = 1;
    return 0;
}
