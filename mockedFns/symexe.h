extern long long __SYMBOLIC(void *var);
extern void __EVAL(); // Don't specify parameters; this way, CIL won't cast them
extern void __ASSERT();
extern void __EVALSTR(char *str, int len);
extern void __COMMENT(char *str);
extern  __attribute__((__nothrow__, __noreturn__)) void exit(int __status );

extern void symtest_initialize(void);
