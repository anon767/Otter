/* Adapted from cil/test/small1/switch_type_conversion.c */

/* the switch controlling expression should be integer promoted, and the case labels should be converted to that type (C99 6.8.4.2) */
typedef enum { A, B } T;
void f(int x){
  switch (x) {
    case '1':
    case 2L:
    case 3:
    case (short)4:
    default:
        break;
  }
}
void g(unsigned y){
  switch (y) {
    case '1':
    case 2L:
    case 3:
    case (short)4:
    default:
        break;
  }
}
void h(T z){
  switch (z) {
    case '1':
    case 2L:
    case 3:
    case (short)4:
    default:
        break;
  }
}
int main(void) {
    f(0);
    g(0U);
    h(A);
    return 0;
}
