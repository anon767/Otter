int max(int x,int y){ 
	return (x>y)?x:y;	
}
void main(){
	int m,n;
	__SYMBOLIC(&m);
	__SYMBOLIC(&n);
	__ASSUME(m<n);
	__ASSERT(max(m,n)==n);
}
