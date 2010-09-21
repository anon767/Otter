// This program should have two execution paths.
#pragma expect_return()
#pragma expect_return()

int main(){
  signed char c;
  __SYMBOLIC(&c);
  if ((int)c < 0);

  return 0;
}
