int main(){

	int x = 0;
	while(1){

		if (x>10) 
			goto Break;
		else
			x = x+1;

	}
Break:
	__ASSERT(x == 11);
	return x;
}

