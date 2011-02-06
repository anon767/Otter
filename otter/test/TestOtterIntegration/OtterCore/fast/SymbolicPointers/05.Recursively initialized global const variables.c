#pragma entry_function("foo")
#pragma no_other_abandoned

void * const q;
void * const p = &q;
void * const q = &p;

void foo(void) {
    __ASSERT(p == &q && q == &p);
}
int main(void) {
    foo();
    return 0;
}
