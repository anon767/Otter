
long __builtin_expect(long x, long expected_value) {
  if (x == expected_value)
    return 0;

  return 1;
/*
  return x;*/
}

#ifndef __SIZE_T
#define __SIZE_T
typedef unsigned int size_t;
#endif

size_t __ctype_get_mb_cur_max() {
  return 1;
}
