#pragma no_other_abandoned

int main(){
	int x,y,z;
	int b;
	x = 1;
	y = 2;
	z = 4;
	b = (x+y<z);
	__ASSERT(b == 1);
	return 0;
}
