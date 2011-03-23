#define MAX_ARGC 8
#define assert(a) if (!(a)) __FAILURE()

void __FAILURE(void) {}

char getchar(void) {
    char c;
    __SYMBOLIC(&c);
    return c;
}

int main(void) {
    int argc; __SYMBOLIC(&argc); __ASSUME(argc >= 0 && argc < MAX_ARGC);
    char argv[MAX_ARGC][1]; __SYMBOLIC(&argv);
    int i, n = 0, b[4] = { 0 };
    for (i = 0; i < argc; i++) {
        if (*argv[i] == 'b') {
            assert(n < 4);
            b[n++];
        }
    }
    for (i = 0; i < 50; i++) {
        if (getchar())
            /* do something */;
    }
    return 0;
}
