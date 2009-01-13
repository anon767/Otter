int f(int a){
	return a*a*a;
}

int main(){

	int x;
	int y = f(x);

	x = y;
	x = y;
	return 0;
}
