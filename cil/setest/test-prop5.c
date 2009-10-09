void main(){

	int a,b;
	__SYMBOLIC(&a);
	__SYMBOLIC(&b);
	__ASSUME(a<0);
	__ASSUME(b>0);


	__ASSERT(a<b);
	__ASSERT((unsigned)a<(unsigned)b);
}
