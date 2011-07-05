/* This test exercises handling short-circuiting operators. The exact behavior
	 depends on whether or not Otter forks at the test of i >= 2. If it does, we
	 should get the following:

	 1. i < 2 and j >= 10 (pass the assertion; return 1)
	 2. i < 2 and j < 10 (return 0)
	 3. i >= 2 and j >= 10 (or j < 0) (out-of-bounds dereference error)
	 4. i >= 2 and j < 10 (return 0)

	 If Otter does *not* fork, the first 'if' still generates path 3 above, but
	 the 'if' evaluates to false, since a[j] is certainly false (if j is in
	 bounds). However, evaluating the if adds to the path condition the
	 implication that *if* i >= 2 *then* j is in bounds. Continuing execution, Otter branches on j < 10 to generate the paths

	 a. j >= 10 (Since j is out-of-bounds, we must have i < 2. Thus, the assertion passes, and we return 1.)
	 b. j < 10 (return 0)

	 Path b is the disjunction of paths 2 and 4 from the forking case. Path a is
	 the same as path 1. Let's call the failing path in this case path c; it is the
	 same as path 3.
*/
// Path 1 or a
#pragma expect_return(j >= 10, i < 2, __return_code__ == 1)
// Path 3 or c
#pragma expect_abandoned(out_of_bounds, j >= 10, i >= 2)
// Path 2, 4, or b
#pragma expect_return(j < 10, __return_code__ == 0)
// In the non-forking case, those are all paths, but in the forking case, either path 2 or path 4 remains unpragma-ed, so we can't say no_other_results.
#pragma no_other_abandoned

char a[10];
int i, j;
int main() {
	__SYMBOLIC(&i);
	__SYMBOLIC(&j);
	if (i >= 2 ? a[j] != 0 : 0) {
		__ASSERT(0); // Unreachable, because a[j] is either out-of-bounds or 0
	}
	if (j < 10) {
		return 0;
	} else {
		// j >= 10, so i must be less than 2; otherwise, a[j] would have been out-of-bounds
		__ASSERT(i < 2);
	}
	return 1;
}
