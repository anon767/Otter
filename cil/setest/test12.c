#define MAX 50

int main(){

	int x; //symbolic
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
	return x;
}

