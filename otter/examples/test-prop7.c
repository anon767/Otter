void main(){
	int n;
	__SYMBOLIC(&n);

	if(n>6)
		n += 1;
	if(n>4)
		n -= 1;

	__ASSERT(n!=6);
}

