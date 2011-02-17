#pragma entry_function("foo")
#pragma no_other_abandoned

void foo(char *t) {
    __ASSERT(!t || *t == 'a' || *t == 'b' || *t == 'c' || *t == '\0');
}

int main(void) {
    foo("abc");
    return 0;
}
