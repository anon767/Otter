#pragma no_other_abandoned

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

int strlen(char *s) {
	char *ss = s;
	while (*ss) {
		ss++;
	}
	return ss - s;
}

int main(){

	int x;
	__SYMBOLIC(&x);
	__ASSUME(x>=0,x<=99999);
	char s[26];
	my_sprintf(s,"%d",x);
	__ASSERT(strlen(s)<=5);

	return 0;
}
