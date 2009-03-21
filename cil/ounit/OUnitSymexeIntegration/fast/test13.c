int main(){

	int input1,input2;
	int most = input1;
	int least = input2;

	if(most<input2)
		most = input2;

	if(least>input1)
		least = input1;

	__ASSERT(least<=most);
	return 0;
}
