#pragma no_other_abandoned

int main(){
	int x,z;
	int* y = &x;
	*y = 10;
	z = x;
	__ASSERT(x == 10, &x == y, *y == 10, z == 10);
	return 0;
}

