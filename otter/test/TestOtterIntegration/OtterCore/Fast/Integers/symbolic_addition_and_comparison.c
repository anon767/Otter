#pragma no_other_abandoned

int main(){

	int x,y,z;
	__SYMBOLIC(&x);
	__SYMBOLIC(&y);

	__ASSUME(x==1);
	__ASSUME(y==260);
	__ASSERT((x<y)-1==0);

	return 0;
}
