#pragma entry_function("foo")
#pragma no_other_abandoned

const char s[] = "abc";
char *t;

void foo() {
    __ASSERT(!t || *t == 'a' || *t == 'b' || *t == 'c' || *t == '\0');
}

int main(void) {
    t = s;
    foo();
    return 0;
}
