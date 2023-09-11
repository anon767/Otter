/*
 * An example showing what compositional SE is aiming at
 */
int fa(int n) { /*...*/ }
int fb(int n) { /*...*/ }
int fc(int n) { /*...*/ }

void main() {
    int n; __SYMBOLIC(&n);
    n = fa(n);
    n = fb(n);
    n = fc(n);
    if (/*...*/)
        assert(0);
}
