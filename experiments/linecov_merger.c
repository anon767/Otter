#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define LINE 100000
#define LEN	 50
#define TARGET "The lines hit were:\n"

char* table[LINE];
int size;

int scmp( const void *sp1, const void *sp2 )
{
	    return( strcmp(*(char **)sp1, *(char **)sp2) );
}

int main(){

	int i,count;
	char buf[1024];
	while(!feof(stdin)){
		fgets(buf,1024,stdin);
		if(strcmp(buf,TARGET)==0){
			while(1){
				fgets(buf,1024,stdin);
				if(strcmp(buf,"\n")==0) break;
				table[size++] = strdup(buf);
				if(size>=LINE){
					printf("Too many lines (try increase the LINE bound)\n");
					exit(1);
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

	


