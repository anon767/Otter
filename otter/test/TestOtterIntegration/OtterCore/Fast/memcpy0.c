/* Check that memcpy does a bounds check on the destination. Ideally, this would
   die with an `OutOfBounds failure, but memcpy is wrapped in
   try_with_job_abandoned_interceptor, so it tries to call a concerete
   implementation, which isn't provided here. As is, this test case at least
   checks that the out-of-bounds write doesn't silently succeed: it doesn't test
   *what* failure happens, but it makes sure that *some* failure happens by
   ensuring that there is no successful return). */

#pragma no_other_return

int main() {
  unsigned int a[1] = {1}, b[2] = {2, 3};
  memcpy(a, b, sizeof(b)); // Write past end of a
  return 0;
}
