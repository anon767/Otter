#pragma no_other_abandoned

typedef int* intptr;

int main() {
    intptr p = (intptr)malloc(sizeof(int)*10);
    p[0] = 0;

    return 0;
}
