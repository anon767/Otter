#ifndef _SETJMP_H
#define _SETJMP_H

/* 
 * setjmp uses a macro to force the allocation of the resulting statement
 * pointer in the calling functions stack.  If the function returns, the
 * statment pointer will also be deallocated, causing a subsequent longjmp to
 * fail.
 *
 * jmp_buf is typedefed as it is because the standard is for it to be
 * an array type. This fact is stripped out before passing control to
 * Otter to simplify the OCaml code. Otter only sees the pointer to
 * int, which points to an internal Otter structure of statment
 * pointers that contains the Cil structures necessary to complete to
 * jump.
 */

/* __libc_setjmp is implemented internally in Otter */
extern int __libc_setjmp(int* s);

typedef int *jmp_buf[1];

#define setjmp(e) ((e)[0] = __builtin_alloca(sizeof(int)), \
	__libc_setjmp((e)[0]))

#endif
