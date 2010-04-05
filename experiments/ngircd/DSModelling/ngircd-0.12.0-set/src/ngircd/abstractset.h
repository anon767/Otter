
#ifndef __abstractset_h__
#define __abstractset_h__

typedef struct _set_elm{
	void* elm;
	struct _set_elm* next;
} __SET_ELM;

typedef struct _set{
	__SET_ELM* head[2]; // 0-internal, 1-external
	void* rest; 
	void* (*clone)(void*); // how to clone an object
} __SET;

void** __ARG(int arg1,...);
void __SET_INIT(__SET* set,void* rest,void* (*clone)(void*));
void __SET_ADD(void* newobj, __SET* set);
//void __SET_ADD_INTERNAL(void* newobj, __SET* set);
void __SET_ITERATE(__SET* set,void (*iterate)(void**,void*),void* pars);
void __SET_FOREACH(void** ret,__SET* set);
int __SET_FIND(void** ret,__SET* set,int (*pred)(void**,void*),void** pars);
int __SET_FIND_CONCRETE(void** ret,__SET* set,int (*pred)(void**,void*),void** pars);
void __SET_REMOVE(void** obj, __SET* set);
int __SET_SIZE(__SET* set);
int __SET_FALSE_PRED(void** pars,void* x);
void* __SET_ABSTRACT_INTERNAL(__SET* set);


void* __PTR_SYMBOLIC(int n);
#define __SYMBOLIC_STR_LEN__   2
void __SYMBOLIC_STRING(char* s);
int __STRING_EQUAL(char* a,char* b);


// make compiler happy
int __SYMBOLIC(int a);
void __CLONE(void* a,void* b,int c);
void __EVAL(int a);
void __EVALSTR(char* a,int len);
void __COMMENT(char* a);
void __ASSERT(int a);
void __ASSUME(int a);
void __ASSUME_SIMPLIFY(int exp);

int AND(int a,int b);
int OR(int a,int b);
int NOT(int a);
int __SYMBOLIC(int a);
int __TRUTH_VALUE(int a);
void __CLONE(void* a,void* b,int c);
#define IMPLY(X,Y)	OR(NOT(X),(Y))

#endif
/* -eof- */
