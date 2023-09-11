#ifdef CIL
    #define MIX(x) __attribute__((mix(x)))
    #define $(x) __attribute__((cilqual(x)))
    #define $null $(null)
    #define $nonnull $(nonnull)
#else
    #define $(x) $ ## x
#endif

#define NULL ((void * $(null))0)

static void ASSERT_NONNULL(void * $(nonnull) x) {}
static void ASSERT_NULL(void * $(null) x) {}
