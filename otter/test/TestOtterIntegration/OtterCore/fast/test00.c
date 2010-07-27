#pragma no_other_abandoned

int main (){

	int x = 10;
	x = x;
	__ASSERT(x==10);
 	return 0;
}
