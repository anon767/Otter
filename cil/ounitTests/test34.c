int main(){
	int x,y;
	__ASSUME(x>=0 , x<=65536);
	__ASSUME(y>=0 , y<=8);
	__ASSERT( x << y >> y == x);
	return 0;
}
