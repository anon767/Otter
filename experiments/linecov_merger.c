#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define LEN	 50
#define TARGET "The lines hit were:\n"

char** table;
int size;
int LINE=10000;

int scmp( const void *sp1, const void *sp2 )
{
	int tmp;
	char* s1 = strdup(*(char**)sp1);
	char* s2 = strdup(*(char**)sp2);
	s1 = strtok(s1,":"); int i1 = atoi(strtok(0,"\0"));
	s2 = strtok(s2,":"); int i2 = atoi(strtok(0,"\0"));
	if((tmp=strcmp(s1,s2))!=0) return tmp;
	else return i1-i2;
}

int main(){

	int i,count;
	char buf[1024];

	table = malloc(sizeof(char*)*LINE);		
	while(!feof(stdin)){
		fgets(buf,1024,stdin);
		if(strcmp(buf,TARGET)==0){
			while(1){
				fgets(buf,1024,stdin);
				buf[strlen(buf)-1] = '\0';
				if(strcmp(buf,"")==0) break;
				table[size++] = strdup(buf);
				if(size>=LINE){
					LINE*=2;
					table = realloc(table,sizeof(char*)*LINE);
				}
			}
		}
	}
	qsort(table,size,sizeof(char*),scmp);
	printf("%s\n",table[0]);
	for(i=1,count=1;i<size;i++){
		if(strcmp(table[i],table[i-1])!=0) {
			count++;
			printf("%s\n",table[i]);
		}
	}
	printf("With repeat: %d\n",size);
	printf("Without repeat: %d\n",count);
	return 0;
}

