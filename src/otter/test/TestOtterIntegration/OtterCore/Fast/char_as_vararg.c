#pragma no_other_abandoned

#ifndef CIL
#include <stdio.h>
#define __ASSERT(x) printf((x) ? "ok\n" : "error\n")
#endif

void f(int ignore,...) {
  __builtin_va_list args;
  __builtin_va_start(args, ignore);
   int x = __builtin_va_arg(args, int);
	__ASSERT(x==1);
  __builtin_va_end(args);
}

int main() {
  char c = 1;
  f(0, c);
	return 0;
}
