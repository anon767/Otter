#pragma no_other_abandoned

int main() {
  int x;
  int *ptr_array[2] = {&x,0};
  unsigned char i;
  __SYMBOLIC(&i);
  __ASSUME(i < 2);
  ptr_array[i] = &x; // Write a nonzero value at a symbolic offset
  __ASSERT(ptr_array[0]); // ptr_array[0] is definitely &x, so this should pass.
  return 0;
}
