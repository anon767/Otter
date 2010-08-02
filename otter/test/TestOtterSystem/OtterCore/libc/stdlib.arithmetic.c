#include <stdlib.h>
#include <limits.h>

int main()
{
	int i;
	long l;
	long long ll;

	__SYMBOLIC(&i);
	__SYMBOLIC(&l);
	__SYMBOLIC(&ll);

	/* abs doesn't work if the result can't be stored, so we ignore this case here */
	__ASSUME(i != INT_MIN);
	__ASSUME(l != LONG_MIN);
	__ASSUME(ll != LLONG_MIN);

	__ASSERT(abs(i) >= 0);
	__ASSERT(labs(l) >= 0);
	__ASSERT(llabs(ll) >= 0);

	int ij = __SYMBOLIC(sizeof(int));
	long lj = __SYMBOLIC(sizeof(long));
	long long llj = __SYMBOLIC(sizeof(long long));
	/* can't divide by 0 */
	__ASSUME(ij != 0);
	__ASSUME(lj != 0);
	__ASSUME(llj != 0);

	div_t di = div(i, ij);
	ldiv_t dl = ldiv(l, lj);
	lldiv_t dll = lldiv(ll, llj);
	
	/* these seem to cause STP to try every possible bit pattern */
	/* Extremely slow -- uncomment only if you have tons of time */
	/*__ASSERT(di.rem + di.quot * ij == i);
	__ASSERT(dl.rem + dl.quot * lj == l);
	__ASSERT(dll.rem + dll.quot * llj == ll);*/
	return(0);
}
