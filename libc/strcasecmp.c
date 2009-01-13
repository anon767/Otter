/*
 * strcasecmp.c
 */

#include <string.h>
//#include "ctype.h"

/**/
// int ITE(int a,int b,int c){
// 	int r =  OR(AND(a,b),AND(NOT(a),c));
// 	__EVAL(r);
// 	return r;
// }
// 
// int toupper2(int c){
// 	int r; //fresh
// 	__ASSUME(ITE(AND(c>='a',c<='z'),r==(c-32),r==c));
// 	return r;
// }

// return non-zero if not equal; don't distinguish > and <
//int strcasecmp(const char *s1, const char *s2)
//{
//	const unsigned char *c1 = (const unsigned char *)s1;
//	const unsigned char *c2 = (const unsigned char *)s2;
//	int d = 0;
//	int fact = 1;
//
//	// assume s1 is a concrete string (as in ngIRCd)
//	// assume s2 is long enough (which is true in SE -- arrays have unbounded length)
//	while (1) {
//		d = toupper2(*c1++) - toupper(*c2++);
//		//d = (*c1++) - (*c2++);
//		fact = fact && (d==0);
//		if(*c1=='\0') break;
//	}
//	return !fact;
//}
/**/

// ORIGINAL:
int strcasecmp(const char *s1, const char *s2)
{
	const unsigned char *c1 = (const unsigned char *)s1;
	const unsigned char *c2 = (const unsigned char *)s2;
	unsigned char ch;
	int d = 0;

	while (1) {
		/* toupper2() expects an unsigned char (implicitly cast to int)
		   as input, and returns an int, which is exactly what we want. */
		d = toupper2(ch = *c1++) - toupper(*c2++);
		if (d || !ch)
			break;
	}

	return d;
}
