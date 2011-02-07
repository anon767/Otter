/*  This tests that Bytes_Conditional pointers:
        - causes a fork upon dereference, for the null and non-null nodes;
        - in the non-null fork, will have the null node properly pruned away.

    In this file, this case occurs when dereferencing *y in foo(): y is a Bytes_Conditional pointer initialized by
    SymbolicPointers. To work correctly, Bytes_Conditional leaf nodes inconsistent with the path condition
    must be pruned away in BytesUtility.bytes__read before reaching Expression.deref.

    There have been several symptoms:
        -   without specifically handling Bytes_Conditional during dereferences before r10002, this leads to a single
            "Dereference something not an address" error, because no fork occurs and the null value in y is not
            pruned away;
        -   with the ConditionalException method of handling Bytes_Conditional from r10002 that did not properly
            propagate the most recent state consistent with the failing condition, this leads to an infinite fork loop,
            because the null value in y could not be properly pruned away due to mismatch between the failing condition
            from first ConditionalException due to the bad dereference and the earlier state before that first
            ConditionalException, leading to Statement.step restarting repeatedly;
        -   with the error channel method of handling Bytes_Conditional from r10032, this leads to a "Error: write to
            a string literal" error, because the null value in y is replaced by a dummy string block (actually from
            r10002) which in turn is not properly pruned away before being written to.
*/

#pragma entry_function("foo")
#pragma time_limit(1)
#pragma expect_abandoned(failure("Dereference something not an address")) /* for the null leaf node */
#pragma expect_return() /* for the non-null leaf node */
#pragma no_other_results

int x = 0;
int * y = &x;
void foo(void) {
    *y = 1;
}
int main(void) {
    return 0;
}

