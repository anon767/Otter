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


/* assume/asserts */
#define SYMTEST_ASSUME_ANY(x) __ASSUME(1)
#define SYMTEST_ASSUME_LT(x, y) __ASSUME(*(x) < (y))
#define SYMTEST_ASSUME_GT(x, y) __ASSUME(*(x) > (y))

#define SYMTEST_ASSERT_TRUE(condition) __SYMTEST_ASSERT(SYMTEST_ASSERT_TRUE(condition),condition)
#define SYMTEST_ASSERT_LT(x, y) __SYMTEST_ASSERT(SYMTEST_ASSERT_LT(x, y),(x) < (y))
#define SYMTEST_ASSERT_GT(x, y) __SYMTEST_ASSERT(SYMTEST_ASSERT_GT(x, y),(x) > (y))

#define SYMTEST_ASSERT_STR_EQUALS(x, y) __SYMTEST_ASSERT(SYMTEST_ASSERT_STR_EQUALS(x, y),!strcmp((x), (y))) 
#define SYMTEST_ASSERT_STR_BEGINS(x, y) __SYMTEST_ASSERT(SYMTEST_ASSERT_STR_BEGINS(x, y),!strncmp((x), (y), strlen(y)))

/* unstub main */
#ifdef main
#undef main
#endif

#endif
