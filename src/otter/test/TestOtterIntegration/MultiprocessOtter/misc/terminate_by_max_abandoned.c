#pragma time_limit(1)
#pragma max_abandoned(1)

int main() {
    int x;
    __SYMBOLIC(&x);
    __otter_multi_fork();
    if (x) __ASSERT(0);
    __ASSERT(0);
    return 0;
}
