/* Adapted from cil/test/small1/va_list_ptr.c */

void foo(__builtin_va_list * x) {
}
int main(void) {
    __builtin_va_list y;
    foo(&y);
    return 0;
}
