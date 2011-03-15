#pragma expect_abandoned(out_of_bounds) /* __SYMBOLIC() % 4 may be negative */
#pragma no_other_abandoned

int foo0() {return 0;}
int foo1() {return 1;}
int foo2() {return 2;}
int foo3() {return 3;}

typedef int (*FP)();

int main()
{
	FP a[4];
	a[0] = foo0;
	a[1] = foo1;
	a[2] = foo2;
	a[3] = foo3;
	
	int x = a[__SYMBOLIC() % 4]();

	__ASSERT(x >= 0);
	__ASSERT(x <= 4);

	return 0;
}
