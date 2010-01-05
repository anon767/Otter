int strlen(char *s) {
	char *ss = s;
	while (*ss) {
		ss++;
	}
	return ss - s;
}

int main(){
	int MAX = 40;
	char dst[MAX],str[MAX];
	int i;

	for (i=0;i<MAX;i++) {
		str[i] = __SYMBOLIC();
	}
	str[MAX-1] = '\0';

	for(i=0;i<MAX && str[i]!='\0';i++)
		dst[i] = str[i];

	dst[i] = '\0';
	__ASSERT(strlen(dst) == strlen(str));
	return 0;
}
