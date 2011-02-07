#ifdef NDEBUG
	#define assert(b) ((void) (0))
#else
	#define __assertQ_(x) #x
	#define __assertQ(x) __assertQ_(x)
	#define assert(b) if((b)==0){__libc_failwith("Assertion failed: " #b " at " __FILE__ ":" __assertQ(__LINE__));}
#endif

#ifndef _ASSERT_H
#define _ASSERT_H
void __libc_failwith(char *msg);
#endif
