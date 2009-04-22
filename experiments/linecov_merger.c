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
	    return( strcmp(*(char **)sp1, *(char **)sp2) );
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
				if(strcmp(buf,"\n")==0) break;
				table[size++] = strdup(buf);
				if(size>=LINE){
					LINE*=2;
					table = realloc(table,sizeof(char*)*LINE);
				}
			}
		}
	}
	qsort(table,size,sizeof(char*),scmp);
	printf("With repeat: %d\n",size);
	for(i=1,count=1;i<size;i++){
		if(strcmp(table[i],table[i-1])!=0) count++;
	}
	printf("Without repeat: %d\n",count);
	return 0;
}

