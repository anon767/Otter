// Remember to run with -Ilibc
#ifndef __OTTER_H__
#define __OTTER_H__
#include "__otter/all.h"
int __COMMENT() { return 0;}
int __ASSERT() { __COMMENT("Body of __ASSERT"); return 0;}
int __SYMBOLIC() { __COMMENT("Body of __SYMBOLIC"); return 0;}
int __EVAL() { __COMMENT("Body of __EVAL"); return 0;}
void* malloc() { __COMMENT("Body of malloc"); return 0;}
int __FAILURE() { __COMMENT("Body of __FAILURE"); return 0;}
#endif
