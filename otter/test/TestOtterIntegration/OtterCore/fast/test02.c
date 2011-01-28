#pragma no_other_abandoned

int main(){
	int x,z;
	int* y = &x;
	*y = 10;
	z = x;
	__ASSERT(x == 10);
	__ASSERT(&x == y);
	__ASSERT(*y == 10);
	__ASSERT(z == 10);
	return 0;
}

