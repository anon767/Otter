#ifndef CIL
#include<stdio.h>
#define __ASSERT(X) 	printf("%-40s : %s\n",#X,(X)?"True":"False");
#endif

int main() {
	unsigned a[2] = { 0x40000000, 0x3fffffff };
	// { -1073741824, 1073741823 } as 31-bit integers
	// {  1073741824, 1073741823 } as 32- or 64-bit integers
	unsigned b[1] = { 0x40000000 };

	// The problem only crops up when doing a binary operation where at least one operand
	// is not a Bytes_Constant, and at least one causes problem when Int64.to_int is used.

	__ASSERT(a[0] == 0x40000000); // true -- without a binary operation, we're fine
	__ASSERT(0x40000000 + 1 == 0x40000001); // true -- with actual numbers, fine
	__ASSERT(a[0] + 1 == 0x40000001); // false -- HERE'S THE PROBLEM

	__ASSERT(0x40000001 == a[0] + 1); // false -- order doesn't matter
	__ASSERT(a[0] == 0x40000001 - 1); // true -- the binary operation has to be on the variable to cause trouble

	// Having 'a' be more than one element is critical, because this makes it a ByteArray (I think).
	// Notice that 'b' doesn't cause a problem
	__ASSERT(a[0] == b[0]); // true
	__ASSERT(b[0] + 1 == 0x40000001); // true
	__ASSERT(a[0] + 1 == b[0] + 1); // false

	// I don't exactly know how casting works
	__ASSERT(a[0] + 1 == -1073741823); // false
	__ASSERT(a[0] + 1 == -1073741823L); // true


	__ASSERT(a[1] == 0x3fffffff); // true
	__ASSERT(0x3fffffff + 1 == 0x40000000); // true -- again, literal numbers don't cause trouble
	__ASSERT(a[1] + 1 == 0x40000000); // true -- no problem with unsigneds under 0x40000000


	// transitivity doesn't hold. This case is weird. The first line works as follows:
	// a[0] is cast to an ocaml int (-1073741824) before the subtraction is done in
	// Int64 arithmetic: -1073741824L - 1L = -1073741825L.
	// Then, when trying to compare against a[1], *which is not a Bytes_Constant*,
	// -1073741825L is against cast to an ocaml int, giving 0x3fffffff, so it == a[1].
	//
	// The third line doesn't work because, since 0x3fffffff is a Bytes_Constant, as is
	// the result of the subtraction (-1073741825L), they are compared directly as Int64s,
	// so they are not found to be equal.
	__ASSERT(a[0]-1 == a[1]); // true
	__ASSERT(a[1] == 0x3fffffff); // true
	__ASSERT(a[0]-1 == 0x3fffffff); // false


	// I don't know if this shows anything new
	__ASSERT(a[1] == a[0]-1); // true
	__ASSERT(a[1]+1 == a[0]); // true
	__ASSERT(a[1]+1-1 == a[1]); // true
	__ASSERT(a[1] + 1 - 1 == a[0] - 1); //false



	// Other casting weirdness. For binops, arguments are 'cast' to ocaml ints then to ocaml
	// Int64s (i.e., C 'long long's). Arithmetic is done as Int64.
	// I don't quite understand why (... = x) can fail when (... = xL) works.
	__ASSERT(a[0] - 0x3fffffff == 1); // false
	__ASSERT(a[0] - 0x3fffffff == -2147483647); // false: -2147483647 prints as 2147483649U
	__ASSERT(a[0] - 0x3fffffff == -2147483647L); // true: -2147483647L prints as 2147483649UL

	// same, even with a[1] instead of 0x3fffffff
	__ASSERT(a[0] - a[1] == 1); // false
	__ASSERT(a[0] - a[1] == -2147483647); // false
	__ASSERT(a[0] - a[1] == -2147483647L); // true

	// same, even with (a[0]+1) instead of 0x40000001
	__ASSERT(a[0] - 0x40000001 == -1); // false
	__ASSERT(a[0] - 0x40000001 == -1L); // true

	__ASSERT(a[0] - (a[0]+1) == -1); // false
	__ASSERT(a[0] - (a[0]+1) == -1L); // true

	__ASSERT(a[0] - 0x40000000 == -2147483648L); // false: -2147483648L prints as (-0x7FFFFFFF-1)
	__ASSERT(a[0] - 0x40000000 == 0); // true

	return 0;
}
