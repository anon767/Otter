extern long long __SYMBOLIC(void);
extern void __EVAL(); // Don't specify parameters; this way, CIL won't cast them
extern void __EVALSTR(char* str, int len);
extern  __attribute__((__nothrow__, __noreturn__)) void exit(int __status );
