#ifdef CIL
#define MIX(x) __attribute__((mix(x)))
#define $(x) __attribute__((cilqual(x)))
#define $null $(null)
#define $nonnull $(nonnull)
#else
#define $(x) $ ## x
#endif
#define NULL ((void * $(null))0)

//void free(void * $(nonnull) p);
char * strchr(const char * $(nonnull), int);
//int strcmp(char const * $(nonnull), char const * $(nonnull));
//char * strtok(char * str, const char * $(nonnull) sep);




