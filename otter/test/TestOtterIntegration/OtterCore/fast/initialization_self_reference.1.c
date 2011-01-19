#pragma no_other_abandoned

struct s { int xs[5]; int *a; };
struct s s = { { 5, 4, 3, 2, 1 }, &s.xs[2] };
int main() {
	__ASSERT(s.a - s.xs == 2);
	return 0;
}
