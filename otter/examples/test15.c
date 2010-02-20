#define MAX 40

int main(){

	char dst[MAX],str[MAX];
	int i;
	for(i=0;i<MAX && str[i]!='\0';i++)
		dst[i] = str[i];

	dst[i] = '\0';

	return 0;
}
