// This program should have two execution paths.
// It doesn't because of Otter's ImmutableArray's default values: x is
// considered to be the same byte repeated 4 times, which can't be the
// integer 1.
#pragma expect_return()
#pragma expect_return()

int main() {
	int x = __SYMBOLIC(1);
	if (x == 1);
	return 0;
}
