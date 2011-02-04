int main() {
    int n;
    int* a = &n;
    int* b = 0;
    int c = (int)(a - b);
    return c;
}

