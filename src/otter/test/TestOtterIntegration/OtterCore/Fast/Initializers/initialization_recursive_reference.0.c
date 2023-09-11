#pragma no_other_abandoned

void *q;
void *p = &q;
void *q = &p;

int main() {
    __ASSERT(p == &q);
    __ASSERT(q == &p);
    return 0;
}
