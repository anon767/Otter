/* (partly) based on apr-abts: http://svn.apache.org/repos/asf/apr/apr/trunk/test/abts.h */

/* include this file in the test-suite file */

#ifndef SYMTEST_H
#define SYMTEST_H

#include <stdio.h>
#include <string.h>

#ifndef CIL
#define __ASSUME(...)
#define __SYMTEST_ASSERT(msg, cond)  do { fprintf(stderr, "%s:%i: %s... %s\n", __FILE__, __LINE__, #msg, (cond) ? "success" : "FAIL"); } while(0)
#define symtest_run_test(test, param) do { fprintf(stderr, "Running %s...\n", #test); test(param); } while(0)
#else
#define __SYMTEST_ASSERT(msg, cond) __ASSERT(cond)
#define symtest_run_test(test, param) test(param)
#endif

#endif
