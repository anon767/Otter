#pragma cil_options("--useLogicalOperators")
#pragma no_other_abandoned

int main() {
	int *a[] = {0}, i;
	__SYMBOLIC(&i);
	__ASSUME(i==0);
	int *p = a[i];
	__EVAL(p);
	return p && *p; // p is 0, so *p should not be evaluated
}
