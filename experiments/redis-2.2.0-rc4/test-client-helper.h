#ifndef TEST_CLIENT_HELPER_H
#define TEST_CLIENT_HELPER_H

#include "deps/hiredis/hiredis.h"

/*
 * Match string against the extended regular expression in
 * pattern, treating errors as no match.
 *
 * Return 1 for match, 0 for no match.
 */
int match(const char *string, char *pattern);

/* Assert that the reply contains an integer, and return that integer */
long long get_int(redisReply *reply);

/* Assert that the reply contains an error matching the pattern */
void expect_error(redisReply *reply, const char *pattern);

/* Assert that the key has the expected encoding */
void expect_encoding(const char *expected_encoding, const char *key);

/* The tcl tests don't differentiate between nil and the empty list or between
   singleton lists and single values, so expect and expect_nil accept either. */

/* Assert that the reply is nil or an empty list */
void expect_nil(redisReply *reply);

/* Assert that the reply contains the given value or a singleton list containing that value. */
void expect(redisReply *reply, const char *expected);

/* Create a list with the given name and with entries specified as a
   space-separated list of strings. Also, assert that the list has the desired
   encoding. */
void create_ziplist(const char *name, const char *entries);
void create_linkedlist(const char *name, const char *entries);

/* Fork a child process which calls f and then exits. The parent waits for that
   to finish before continuing. */
void run_in_child_process(void (*f)(void));

#endif
