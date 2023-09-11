#define LEN 256

int isalnum(int i){
	return (i>='0' && i<='9') || (i>='A' && i<='z');
}

void main(){
	int i;
	char s[LEN];
	for(i=0;i<LEN-1;i++){
		char symb = __SYMBOLIC();
		__ASSUME(isalnum(symb));
		s[i] = symb;
	}
	s[LEN-1] = '\0';
	__ASSERT(isalnum(s[10]));
}

