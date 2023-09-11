/* the switch controlling expression should be integer promoted, and the case labels should be converted to that type (C99 6.8.4.2) */
typedef enum { A, B } T;
void f(int x, unsigned y, T z){
  switch (x) {
    case '1':
    case 2L:
    case 3:
    case (short)4:
    default:
        break;
  }
  switch (y) {
    case '1':
    case 2L:
    case 3:
    case (short)4:
    default:
        break;
  }
  switch (z) {
    case '1':
    case 2L:
    case 3:
    case (short)4:
    default:
        break;
  }
}
