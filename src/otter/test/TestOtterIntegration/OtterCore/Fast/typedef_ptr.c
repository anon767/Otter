#pragma no_other_abandoned

typedef unsigned long size_t;
void * malloc(size_t size);
typedef int* intptr;

int main() {
    intptr p = (intptr)malloc(sizeof(int)*10);
    p[0] = 0;

    return 0;
}
