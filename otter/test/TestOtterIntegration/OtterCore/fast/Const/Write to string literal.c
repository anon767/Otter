#pragma expect_abandoned(failure("Error: write to a const"))
#pragma no_other_results

char *x = "abc";
int main() {
    x[0] = 'd';
    return 0;
}
