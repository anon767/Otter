#ifndef __OTTER_H__
#define __OTTER_H__
void __COMMENT(char* x) { __COMMENT("Body of __COMMENT"); }
void __ASSERT(int x) { __COMMENT("Body of __ASSERT"); }
void __SYMBOLIC(void* x) { __COMMENT("Body of __SYMBOLIC"); }
void __EVAL(int x) { __COMMENT("Body of __EVAL"); }
void* malloc(int x) { __COMMENT("Body of malloc"); }
#endif
