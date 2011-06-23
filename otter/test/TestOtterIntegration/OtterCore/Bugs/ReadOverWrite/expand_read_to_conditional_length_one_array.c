/*  This tests that pointer arrays of length one can be read with a symbolic index and the resulting pointer can be
    dereferenced.

    As of r13006, this test fails with a "Not a valid array" error, since BytesUtility.expand_read_to_conditional2
    incorrectly assumes that arrays are always represented by Bytes.Bytes_ByteArray, which is not the case for
    length-one arrays.
*/

#pragma expect_return()
#pragma no_other_results

int main(void) {
    int i = 1;
    int *x[] = { &i };

    int k = 0;
    __SYMBOLIC(&k);
    __ASSUME(k == 0);

    int *q = x[k];
    __ASSERT(*q == 1);

    return 0;
}
