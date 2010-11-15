/*
 * ./otter.pl --dobackotter -Ilibc --max-abandoned=1 test/TestBackOtter/dfa_hybrid.c
 *
 * Failing path: Decision: IF ((int )c == 93): true
 *               Decision: state8: void (void)
 *               Decision: IF ((int )c == 41): true
 *               Decision: state7: void (void)
 *               Decision: IF ((int )c == 125): true
 *               Decision: state6: void (void)
 *               Decision: IF ((int )c == 120): true
 *               Decision: state5: void (void)
 *               Decision: IF ((int )c == 97): true
 *               Decision: state4: void (void)
 *               Decision: IF ((int )c == 126): true
 *               Decision: state3: void (void)
 *               Decision: IF ((int )c == 123): true
 *               Decision: state2: void (void)
 *               Decision: IF ((int )c == 40): true
 *               Decision: state1: void (void)
 *               Decision: IF ((int )c == 91): true
 *               Decision: state0: void (void)
 */

#include "otter.h"

char input(void) {
    char c;
    __SYMBOLIC(&c);
    return c;
}

// input() is unfolded, as currently the distance_to_target metric does not work with function call.
void state8(void) { char c; __SYMBOLIC(&c); if (c == ']')  __FAILURE(); else state8(); }
void state7(void) { char c; __SYMBOLIC(&c); if (c == ')')  state8();    else state7(); }
void state6(void) { char c; __SYMBOLIC(&c); if (c == '}')  state7();    else state6(); }
void state5(void) { char c; __SYMBOLIC(&c); if (c == 'x')  state6();    else state5(); }
void state4(void) { char c; __SYMBOLIC(&c); if (c == 'a')  state5();    else state4(); }
void state3(void) { char c; __SYMBOLIC(&c); if (c == '~')  state4();    else state3(); }
void state2(void) { char c; __SYMBOLIC(&c); if (c == '{')  state3();    else state2(); }
void state1(void) { char c; __SYMBOLIC(&c); if (c == '(')  state2();    else state1(); }
void state0(void) { char c; __SYMBOLIC(&c); if (c == '[')  state1();    else state0(); }

int main(void) {
    state0();
    return 0;
}
