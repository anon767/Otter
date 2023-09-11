/* Test that fork returns the correct values to parent and child */
#pragma expect_return()
#pragma expect_return()
#pragma no_other_results

int main(char** argc, int argv) {
    int parent_pid = __otter_multi_get_pid();
    int fork_retval = __otter_multi_fork();
    if (__otter_multi_get_pid() == parent_pid) {
        // fork should return nonzero to the parent
        __ASSERT(fork_retval != 0);
    } else {
        // fork should return 0 to the child
        __ASSERT(fork_retval == 0);
    }
    return 0;
}
