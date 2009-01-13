#include<stdarg.h>
//#include<stdarg_fixed.h>

int impl(int num,va_list s){

	int i;
	int min = 0x7fffffff;
	int n;

	for (i = 0; i < num; i++){
		n = va_arg(s, int);
		if (n<min) min = n;
	}
	return min;
}

int minimum (int num,...){
	va_list s;
	int r;

	va_start(s, num); 

	r = impl(num,s);

	va_end(s);

	return r;
}

int main(){

	int i1=1,i2=2,i3=3;
	//int i1,i2,i3;
	int min = minimum (3,i1,i2,i3);
	__ASSERT(min<=i1,min<=i2,min<=i3);
	return 0;
}
