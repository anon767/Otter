#line 2 "__otter_poi.c"
/** Points of interests */

#pragma cilnoremove("__FAILURE")
void __FAILURE(void) {}

#pragma cilnoremove("__otter_xalloc_die_failure")
void __otter_xalloc_die_failure(void) {
#ifdef __OTTER_XALLOC_DIE_FAILURE
    __FAILURE();
#endif
}

#pragma cilnoremove("__otter_quotearg_buffer_restyled_assert")
void __otter_quotearg_buffer_restyled_assert(int truth) {
#ifdef __OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT
    if (!truth) __FAILURE();
#endif
}

#pragma cilnoremove("__otter_paste_assert")
void __otter_paste_assert(int truth) {
#ifdef __OTTER_PASTE_ASSERT
    if (!truth) __FAILURE();
#endif
}

