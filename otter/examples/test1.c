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
	x = obj.a[50];
	return 0;
}
