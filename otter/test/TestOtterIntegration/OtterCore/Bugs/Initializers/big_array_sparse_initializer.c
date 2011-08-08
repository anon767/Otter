/*  This tests the speed of initializing large arrays. As of r13436, this test takes a very long time to complete due
    to the slowness of hashconsing ImmutableArray, because:
    -   its hash function is particularly slow as it hashes every element to reduce hash collisions, since hashing
        just the first 20 elements leads to too many collisions, likely because many arrays begin with similar
        contents (e.g., zeros);
    -   its equal function compares every element (in most cases), in part due to the complexity of handling default
        values in a sane manner, and largely due to the inability to compare arrays in larger chunks.

    This particular test case primarily exercises the hash function since it initializes only one array. It may be
    optimized by taking advantage of that the array contains mostly 0.
 */
#pragma time_limit(5)
#pragma expect_return(__return_code__ == 0)
#pragma no_other_results

int a[] = { [9999] = 1 };

int main(void) {
    return 0;
}