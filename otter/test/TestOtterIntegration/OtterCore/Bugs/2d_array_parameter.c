#pragma no_other_abandoned

int f(int a[][2]) {
	return a[0][1];
	// Replacing the previous line with the (equivalent) next two works fine.
	// int *b = a[0];
	// return b[1];
}
int main() {
	int a[2][2]={1,2,3,4};
	return f(a);
}
