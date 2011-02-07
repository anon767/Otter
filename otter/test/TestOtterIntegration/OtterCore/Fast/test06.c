#pragma no_other_abandoned

int main(){

	int x,y[100],*z1,*z2;
	y[10] = 128;
	z1 = y+30;
	z2 = z1-20;
	x = *z2;  // x=128
	__ASSERT(x == 128);
	return 0;
}
