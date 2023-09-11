/* A stub for longjmp for Otter. See
 * libc/machine/otter/machine/setjmp.h for setjmp.
 *
 * __libc_longjmp is implemented internally in Otter.
 *
 * TODO: it should be possible to dispense with __libc_longjmp and
 * just have Otter handle longjmp directly. The same goes for setjmp.
 */

#include <machine/setjmp.h>

void __libc_longjmp(int *, int);

void longjmp(jmp_buf environment, int value)
{
	__libc_longjmp(environment[0], value ? value : 1);
}
