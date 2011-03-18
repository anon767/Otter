#line 2 "__otter_poi.c"
/** Points of interests */
#include <string.h>

#pragma cilnoremove("__FAILURE")
void __FAILURE(void) {}

#pragma cilnoremove("__otter_poi_noop")
void __otter_poi_noop(void) {
    // Some instructions to be covered
    int x;
    x = 1;
    x++;
    return;
}

// TODO: remove this
#pragma cilnoremove("__otter_xalloc_die_failure")
void __otter_xalloc_die_failure(void) {
#ifdef __OTTER_XALLOC_DIE_FAILURE
    __FAILURE();
#endif
}

#pragma cilnoremove("__otter_quotearg_buffer_restyled_assert")
void __otter_quotearg_buffer_restyled_assert(int truth) {
#ifdef __OTTER_QUOTEARG_BUFFER_RESTYLED_ASSERT
    if (!truth) 
        __FAILURE();
#endif
}

#pragma cilnoremove("__otter_paste_assert")
void __otter_paste_assert(int truth) {
#ifdef __OTTER_PASTE_ASSERT
    if (!truth) 
        __FAILURE();
#endif
}

#pragma cilnoremove("__otter_injected_make_node_op_equals_assert")
void __otter_injected_make_node_op_equals_assert(int truth) {
#ifdef __OTTER_INJECTED_MAKE_NODE_OP_EQUALS_ASSERT
    if (!truth) 
        __FAILURE();
#endif
}

#pragma cilnoremove("__otter_long_double_format_bounds_checking")
void __otter_long_double_format_bounds_checking(char const *fmt, int i) {
#ifdef __OTTER_LONG_DOUBLE_FORMAT_BOUNDS_CHECKING
    int len = strlen(fmt);
    if (len < i) 
        __FAILURE();
#endif
}

#pragma cilnoremove("__otter_copy_unescaped_string_bounds_checking")
void __otter_copy_unescaped_string_bounds_checking(char const *string, int i) {
#ifdef __OTTER_COPY_UNESCAPED_STRING_BOUNDS_CHECKING
    int len = strlen(string);
    if (len < i) 
        __FAILURE();
#endif
}



