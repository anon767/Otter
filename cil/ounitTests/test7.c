int main(){
	int x,y[10],*z1,*z2;
	z1 = &y[4];
	z2 = &y[9];
	x = z2-z1; //x = 5
	__ASSERT(x == 5);
	return 0;
}
