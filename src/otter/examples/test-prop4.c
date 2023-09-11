void main(){
	int x,y,x0,y0;
	__SYMBOLIC(&x);
	__SYMBOLIC(&y);
	__ASSUME(x!=y);
	x0 = x;
	y0 = y;
	x ^= y ^= x ^= y;
	__ASSERT(x0==y && y0==x);
}
