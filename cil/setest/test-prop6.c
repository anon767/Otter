void f(char *p){
	p[2] &= 0x0f;
}
void main(){
	int n;
	__SYMBOLIC(&n);
	__ASSUME(n<=0x00ffffff);
	if(n>0x0000ffff){
		f(&n);
	}
	__ASSERT(n<=0x000fffff);
}
