#ifndef _STDARG_H
#define _STDARG_H

#define va_list __builtin_va_list

/* these have special types, so intercepting by name is not really an option. */

#define va_start __builtin_va_start
#define va_end __builtin_va_end
#define va_arg __builtin_va_arg
#define va_copy __builtin_va_copy

#endif
