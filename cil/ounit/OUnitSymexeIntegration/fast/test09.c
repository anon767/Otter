int main(){
	int x,y;
	if(x<0){
		if(x<x+3){
			y = 0;
		}
		else 
			__ASSERT(0);

	}
	else{
		y = -1;

	}
	__ASSERT(OR(AND(x < 0, y == 0), AND(x >= 0, y == -1)));
	return 0;
}
