#ifndef _SETJMP_H
#define _SETJMP_H

/* 
 * setjmp uses a macro to force the allocation of the resulting statement
 * pointer in the calling functions stack.  If the function returns, the
 * statment pointer will also be deallocated, causing a subsequent longjmp to
 * fail.
 *
 * jmp_buf if typedefed as it is because the standard is for it to be an struct
 * array type.  This fact is stripped out before passing control to Otter,
 * since the structure is difficult to deal with in Otter.  Instead the int* s
 * points to an internal Otter structure of statment pointers that contains the
 * Cil structures necessary to complete to jump.
 */

#define setjmp(e) ((e)[0].s = __builtin_alloca(sizeof(int)), \
	__libc_setjmp((e)[0].s))

struct __libc_jup_buf
{
	int *s;
};

typedef struct __libc_jup_buf jmp_buf[1];

int __libc_setjmp(int* s);
void __libc_longjmp(jmp_buf environment, int value);

void longjmp(jmp_buf environment, int value)
{
	__libc_longjmp(environment[0].s, value ? value : 1);
}

#endif
