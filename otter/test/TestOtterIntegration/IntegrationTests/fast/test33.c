void my_sprintf(char *dest, const char *ignored, int n) {
	if (n == 0) {
		dest[0] = '0';
		dest[1] = 0;
		return;
	}
	if (n < 0) {
		dest[0] = '-';
		dest++;
		n = -n;
	}
	int i, m;
	for (i = 0, m = n; m > 0; m /= 10) {
		i++;
	}
	dest[i] = 0;
	while (i > 0) {
		i--;
		dest[i] = (n % 10) + '0';
		n /= 10;
	}
	return;
}

int main(){


	char s[26];

	my_sprintf(s,"%d",365);
	__ASSERT(s[0]=='3',s[1]=='6',s[2]=='5',s[3]==0);

	return 0;
}
