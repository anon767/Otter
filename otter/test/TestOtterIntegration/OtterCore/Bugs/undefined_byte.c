#pragma no_other_abandoned

/* The assertion causes a failure because the array has the value
	 Write([undef],i,0). When Otter tries to translate the array
	 to STP, it hits 'undef' and raises an error. */
int main(){
  char a[1];
  unsigned char i;
  __SYMBOLIC(&i);
  __ASSUME(i == 0);
  a[i] = 0;
  __ASSERT(a[0] == 0);

  return 0;
}
