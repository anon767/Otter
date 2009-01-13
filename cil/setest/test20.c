
int absdiff(int x,int y){
	if(x<y)
		return y-x;
	else
		return x-y;
}


int main (){

	int x,y;

	__ASSUME(x>=0);
	__ASSUME(y>=0);
	
	__ASSERT(absdiff(x,y)>=0);

	return 0;
}
