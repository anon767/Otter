int main(){
	char c;
	int i = 2561;
	c = (char)i;
	c = c;
	__ASSERT(c == 1);

	return 0;
}
