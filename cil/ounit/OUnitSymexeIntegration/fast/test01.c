struct s
{
	int n;
	int a[100];
	double d;
};

int main() {
	int x = 100;
	struct s obj;
	obj.a[50] = x;
	__ASSERT(obj.a[50] == 100);
	x = obj.a[50];
	__ASSERT(100 == x);
	return 0;
}
