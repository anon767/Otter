#include <stdarg.h>
void foo(va_list * x) {
}
void bar(va_list y) {
    foo(&y);
}
