/*
 * An example showing when BackOtter does good
 */

#include "otter.h"

void signal(int s) {
    if (s == 0) {
        // action 0
    }
    else if (s == 1) {
        // action 1
    }
    else if (s == 2) {
        // action 2
    }
    else if (s == 3) {
        // action 3
    }
    else if (s == 4) {
        // action 4
    }
    else {
        __FAILURE();
    }
}

void case0() { signal(0); }
void case1() { signal(1); }
void case2() { signal(2); }
void case3() { signal(3); }
void case4() { signal(4); }
void error() { signal(-1); }

void main() {
    int s; __SYMBOLIC(&s);
    if (s == 0) {
      //LOTS OF WORK
        case0();
    }
    else if (s == 1) {
      //LOTS OF WORK
        case1();
    }
    else if (s == 2) {
      //LOTS OF WORK
        case2();
    }
    else if (s == 3) {
      //LOTS OF WORK
        case3();
    }
    else if (s == 4) {
      //LOTS OF WORK
        case4();
    }
    else {
      //LOTS OF WORK
        error();
    }
}

