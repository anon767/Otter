#pragma expect_return(__return_code__ == 1)
#pragma expect_abandoned(failure("Trying to block on non-shared memory"))
#pragma no_other_results

int main() {
  int x = 0;
  if (__otter_multi_fork()) {
    __otter_multi_io_block(&x); // This should fail because x is not shared memory
    return 0;
  } else {
    x = 1;
    return 1;
  }
}
