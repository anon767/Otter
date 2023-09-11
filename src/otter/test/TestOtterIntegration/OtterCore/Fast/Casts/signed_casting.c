#pragma no_other_abandoned

int main() {
	int x = -1;
	long long y = x;
	__ASSERT(y == -1);
	y = -2LL + x;
	__ASSERT(y == -3);
	unsigned short u = -1;
	y = u;
	__ASSERT(y == 65535);
	signed char c = -1;
	y = c;
	__ASSERT(y == -1);
	
	u = y;
	__ASSERT(u == 65535);
	x = y - 256;
	__ASSERT(x == -257);
	u = x;
	__ASSERT(u == 65535-256);
	__ASSERT(u != -257);
	c = x;
	__ASSERT(c == -1);
	c = x + 357; // 100
	__ASSERT(c == 100);
	c = x + 457; // 200
	__ASSERT(c == 200-256);
	return 0;
}
