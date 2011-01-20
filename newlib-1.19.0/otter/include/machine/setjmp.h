/* 
 * Otter's machine/setjmp.h
 *
 * This header, together with setjmp.h, 
 * must shield the ones inside newlib, 
 * as these two pairs are not compatible.
 */
#ifndef _MACHINE_SETJMP_H_
#define _MACHINE_SETJMP_H_

/* __libc_setjmp is implemented internally in Otter */
extern int __libc_setjmp(int* s);

/*
 * jmp_buf is typedefed as it is because the standard is for it to be
 * an array type. This fact is stripped out before passing control to
 * Otter to simplify the OCaml code. Otter only sees the pointer to
 * int, which points to an internal Otter structure of statment
 * pointers that contains the Cil structures necessary to complete to
 * jump.
 */
typedef int *jmp_buf[1];

void longjmp(jmp_buf __jmpb, int __retval);

/* 
 * setjmp uses a macro to force the allocation of the resulting statement
 * pointer in the calling functions stack.  If the function returns, the
 * statment pointer will also be deallocated, causing a subsequent longjmp to
 * fail.
 */
#define setjmp(e) ((e)[0] = __builtin_alloca(sizeof(int)), \
	__libc_setjmp((e)[0]))

#endif
