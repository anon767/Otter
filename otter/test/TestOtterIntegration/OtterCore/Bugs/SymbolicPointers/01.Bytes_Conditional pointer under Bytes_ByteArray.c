/*  This tests that Bytes_Conditional pointers that are hidden inside Bytes_ByteArray values:
        - are properly revealed;
        - causes a fork upon dereference, for the null and non-null nodes;
        - in the non-null fork, will have the null node properly pruned away.

    In this file, this case occurs when dereferencing *x.i in foo(): x is a Bytes_ByteArray struct, and i is a
    Bytes_Conditional pointer initialized by SymbolicPointers. To work correctly, Bytes_Conditional leaf nodes
    inconsistent with the path condition must be pruned away in BytesUtility.bytes__read before reaching
    Expression.deref.

    There have been several symptoms:
        -   without specifically handling Bytes_Conditional during dereferences before r10002, this leads to a single
            "Dereference something not an address (bytearray)" error, because no fork occurs and the null value in x.i
            is not pruned away;
        -   with the ConditionalException method of handling Bytes_Conditional from r10002, this leads to an infinite
            fork loop, because the null value in x.i is not properly pruned away after the first ConditionalException
            due to the bad dereference, leading to Statement.step restarting repeatedly;
        -   with the error channel method of handling Bytes_Conditional from r10032, this leads to a "Error: write to
            a string literal" error, because the null value in x.i is replaced by a dummy string block (actually from
            r10002) which in turn is not properly pruned away before being written to.
*/

#pragma entry_function("foo")
#pragma time_limit(1)
#pragma expect_abandoned(failure("Dereference something not an address (bytearray)")) /* for the null leaf node */
#pragma expect_return() /* for the non-null leaf node */
#pragma no_other_results

int r = 0;
struct a { char c; int * i; } x = { 'a', &r };
void foo(void) {
    *x.i = 1;
}
int main(void) {
    return 0;
}

