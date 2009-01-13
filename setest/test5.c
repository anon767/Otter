int main(){
	int x,y[10],*z;
	y[0] = 12;
	y[5] = 12;
	z = y+5;
	x = *z;
	return 0;
}
