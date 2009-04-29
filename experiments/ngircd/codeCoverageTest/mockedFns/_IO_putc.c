#include <libio.h>

#ifdef _IO_putc
#undef _IO_putc
#endif
int _IO_putc(int __c , _IO_FILE *__fp ) {
	return 0;
}
