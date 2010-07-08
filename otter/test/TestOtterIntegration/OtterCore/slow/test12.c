int main(){
	int MAX = 10;
	int x; //symbolic
	__SYMBOLIC(&x);
	if(x>=0){
		if(x<=MAX){
			//x = ((((((((x+1)+1)+1)+1)+1)+1)+1)+1);
			while(1){
		
				if (x>MAX) 
					goto Break;
				else
					x = x+1;
			}
		}
	}
Break:
	__ASSERT(OR(x < 0, x > MAX));
	return x;
}

