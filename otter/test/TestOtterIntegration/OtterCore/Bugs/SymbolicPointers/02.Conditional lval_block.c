/*  This tests that conditional lval_block variables:
        - causes a fork upon access;
        - will have nodes inconsistent with the path condition pruned away.

    SymbolicPointers may initialize variables to be mapped to conditional lval_block: in this file, &x is initialized
    to a conditional lval_block at the *x dereference in foo(). To work correctly, lval_block leaf nodes inconsistent
    with the path condition must be pruned away before reaching Expression.deref.

    There have been several symptoms:
        -   without specifically handling Bytes_Conditional during dereferences before r10002, this leads to a single
            "Dereference something not an address" error, because no fork occurs at the first *y dereference;
        -   with the ConditionalException method of handling Bytes_Conditional from r10002 that did not properly
            propagate the most recent state consistent with the failing condition, this leads to an infinite fork loop,
            because the null value in y could not be properly pruned away due to mismatch between the failing condition
            from first ConditionalException due to the bad dereference and the earlier state before that first
            ConditionalException, leading to Statement.step restarting repeatedly;
        -   with the ConditionalException method of handling Bytes_Conditional from r10002 and the partial fix from
            r10008 to propagate the most recent state consistent with the failing condition, this still leads to an
            infinite fork loop: although the null value in y is properly pruned away, the null value in x is not
            properly pruned away, because the conditional guard occurs in the lval_block for x (i.e., the value of &x)
            that is not properly, again leading to Statement.step restarting repeatedly;
        -   with the error channel method of handling Bytes_Conditional from r10032, this leads to a "Error: write to
            a string literal" error, because the null value in y is replaced by a dummy string block (actually from
            r10002) which in turn is not properly pruned away before being written to.
*/

#pragma entry_function("foo")
#pragma time_limit(1)
#pragma expect_abandoned(failure("Dereference something not an address (bytearray)")) /* for the null leaf node of y */
#pragma expect_abandoned(failure("Dereference something not an address (bytearray)")) /* for the null leaf node of x */
#pragma expect_return() /* for the non-null leaf node */
#pragma no_other_results

int r = 0;
int * x = &r;
int ** y = &x;
void foo(void) {
    *y = 0;
    *x = 1;
}
int main(void) {
    return 0;
}

