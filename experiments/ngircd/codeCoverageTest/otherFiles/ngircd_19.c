#include<string.h>
// configtest
// int   myargc 	 = 2;
// char  myargstr[] = "ngircd\0-n\0--configtest\0          ";
// char* myargv[]   = {myargstr,myargstr+7,myargstr+10};
extern int   myargc ;
extern char  myargstr[] ;
extern char* myargv[] ; 

int symtest(){
	
	int t;

	myargc = 3;
	strcpy(myargstr+10,"--help");

	return 0;
}

