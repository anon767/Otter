#include "test-client-helper.h"

#include <otter/otter_builtins.h>
#include <otter/otter_scheduler.h>

#include "deps/hiredis/hiredis.h"

#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include <regex.h>

redisContext *c;

/* This regular expression code was taken from http://pubs.opengroup.org/onlinepubs/009695399/functions/regcomp.html */

/*
 * Match string against the extended regular expression in
 * pattern, treating errors as no match.
 *
 * Return 1 for match, 0 for no match.
 */

int
match(const char *string, char *pattern)
{
    int    status;
    regex_t    re;


    if (regcomp(&re, pattern, REG_EXTENDED|REG_NOSUB) != 0) {
        return(0);      /* Report error. */
    }
    status = regexec(&re, string, (size_t) 0, NULL, 0);
    regfree(&re);
    if (status != 0) {
        return(0);      /* Report error. */
    }
    return(1);
}

void expect_error(redisReply *reply, const char *pattern) {
     __ASSERT(reply->type == REDIS_REPLY_ERROR && match(reply->str, pattern));
}

void expect_encoding(const char *expected_encoding, const char *key) {
    redisReply *reply = redisCommand(c, "DEBUG OBJECT %s", key);
    __ASSERT(reply->type == REDIS_REPLY_STATUS);
    const char *encoding = strstr(reply->str, "encoding");
    __ASSERT(encoding);
    encoding += sizeof("encoding"); // Do not subtract 1 for the null byte, because we also want to skip past the following colon
    __ASSERT(strncmp(encoding, expected_encoding, strlen(expected_encoding)) == 0);
    freeReplyObject(reply);
}

long long get_int(redisReply *reply) {
    __ASSERT(reply->type == REDIS_REPLY_INTEGER);
    return reply->integer;
}

/* The tcl tests do not differentiate nil and the empty list or between singleton lists and single values */

void expect_nil(redisReply *reply) {
    __ASSERT(reply->type == REDIS_REPLY_NIL ||
             reply->type == REDIS_REPLY_ARRAY && reply->elements == 0);
}

int streq(const char *a, const char *b) {
    return strcmp(a, b) == 0;
}

int is_expected_value(redisReply *reply, const char *expected) {
    if (reply->type == REDIS_REPLY_STRING) {
        // It's a string. Is it the correct value?
        __ASSERT(streq(expected, reply->str));
        return 1;
    }
    if (reply->type == REDIS_REPLY_INTEGER) {
        // It's an integer. Is it the correct value?
        errno = 0;
        long long expected_num = strtoll(expected, NULL, 10);
        __ASSERT(errno == 0 && expected_num == reply->integer);
        return 1;
    }
    return 0; // Not a string or an integer
}

void expect(redisReply *reply, const char *expected) {
    __ASSERT(is_expected_value(reply, expected) ||
             reply->type == REDIS_REPLY_ARRAY && reply->elements == 1 && is_expected_value(reply->element[0], expected));
}

/* entries is a space-separated list of strings to put into the list */
void create_list(const char *key, const char *entries, const char *encoding) {
    freeReplyObject(redisCommand(c, "del %s", key));
    char *entry = strtok(entries, " ");
    while (entry) {
        freeReplyObject(redisCommand(c, "rpush %s %s", key, entry));
        entry = strtok(NULL, " ");
    }
    assert_encoding(encoding, key);
}

void create_ziplist(const char *key, const char *entries) {
    create_list(key, entries, "ziplist");
}

void create_linkedlist(const char *key, const char *entries) {
    create_list(key, entries, "linkedlist");
}

void run_in_child_process(void (*f)(void)) {
  int *p = __otter_multi_gmalloc(sizeof(int));
  *p = 0;
  if (!fork()) {
    f();
    *p = 1;
    exit(0);
  }
  __otter_multi_block_while_condition(*p == 0, p);
}
