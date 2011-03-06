#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "deps/hiredis/hiredis.h"

#ifndef LEN
#define LEN 1
#endif

void client_main(void) {
    unsigned int j;
    redisContext *c;
    redisReply *reply;

    c = redisConnect((char*)"127.0.0.1", 6379);
    if (c->err) {
        printf("Connection error: %s\n", c->errstr);
        exit(1);
    }

    /* PING server */
    reply = redisCommand(c,"PING");
    printf("PING: %s\n", reply->str);
    freeReplyObject(reply);

    /* Set a key */
    char foo[LEN+1], foo_value[LEN+1];
    __SYMBOLIC(&foo); foo[LEN] = 0;
    __SYMBOLIC(&foo_value); foo_value[LEN] = 0;

    reply = redisCommand(c,"SET %s %s", foo, foo_value);
    printf("SET: %s\n", reply->str);
    freeReplyObject(reply);

    /* Set a key using binary safe API */
    char bar[LEN], bar_value[LEN];
    __SYMBOLIC(&bar);
    __SYMBOLIC(&bar_value);

    reply = redisCommand(c,"SET %b %b", bar, LEN, bar_value, LEN);
    printf("SET (binary API): %s\n", reply->str);
    freeReplyObject(reply);

    /* Try a GET and two INCR */
    reply = redisCommand(c,"GET %s", foo);
    printf("GET foo: %s\n", reply->str);
    freeReplyObject(reply);

    char counter[LEN+1];
    __SYMBOLIC(&counter); counter[LEN] = 0;
    reply = redisCommand(c,"INCR %s", counter);
    printf("INCR counter: %lld\n", reply->integer);
    freeReplyObject(reply);
    /* again ... */
    char counter2[LEN+1];
    __SYMBOLIC(&counter2); counter2[LEN] = 0;
    reply = redisCommand(c,"INCR %s", counter2);
    printf("INCR counter: %lld\n", reply->integer);
    freeReplyObject(reply);

    /* Create a list of numbers, from 0 to 9 */
    char listkey[LEN+1];
    __SYMBOLIC(&listkey); listkey[LEN] = 0;
    reply = redisCommand(c,"DEL %s", listkey);
    freeReplyObject(reply);
    for (j = 0; j < 10; j++) {
        char buf[LEN];
        __SYMBOLIC(&buf);

        reply = redisCommand(c,"LPUSH %s element-%b", listkey, buf, LEN);
        freeReplyObject(reply);
    }

    /* Let's check what we have inside the list */
    char listkey2[LEN+1];
    __SYMBOLIC(&listkey2); listkey2[LEN] = 0;
    reply = redisCommand(c,"LRANGE %s 0 -1", listkey2);
    if (reply->type == REDIS_REPLY_ARRAY) {
        for (j = 0; j < reply->elements; j++) {
            printf("%u) %s\n", j, reply->element[j]->str);
        }
    }
    freeReplyObject(reply);
}
