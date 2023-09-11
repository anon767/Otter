#include <otter/otter_builtins.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "deps/hiredis/hiredis.h"

extern const char *generate_command(void);

void client_main(void) {
	redisContext *c;
	redisReply *reply;

	c = redisConnect((char*)"127.0.0.1", 6379);
	if (c->err) {
		printf("Connection error: %s\n", c->errstr);
		__ASSERT(0);
	}

	char keep_going;
	do {
		reply = redisCommand(c, generate_command());
		switch (reply->type) {
		case REDIS_REPLY_STATUS:
		case REDIS_REPLY_ERROR:
		case REDIS_REPLY_STRING:
			__EVALSTR(reply->str, reply->len);
			break;
		case REDIS_REPLY_INTEGER:
			__EVAL(reply->integer);
			break;
		case REDIS_REPLY_ARRAY:
			__EVALSTR("REDIS_REPLY_ARRAY", 17);
			break;
		case REDIS_REPLY_NIL:
			__EVALSTR("REDIS_REPLY_NIL", 15);
			break;
		}
		freeReplyObject(reply);
		__SYMBOLIC(&keep_going);
	} while (keep_going);
}
