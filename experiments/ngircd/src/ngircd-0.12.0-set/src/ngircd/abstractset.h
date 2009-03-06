
#ifndef __abstractset_h__
#define __abstractset_h__

typedef struct _set_elm{
	void* elm;
	struct _set_elm* next;
} __SET_ELM;

typedef struct _set{
	__SET_ELM* head[2]; // 0-internal, 1-external
	void* rest; 
	int (*rest_constraint)(void*); // constraints that comes with rest (but not in PC)
	void* (*clone)(void*); // how to clone an object
} __SET;

void** __ARG(int arg1,...);
int __SET_DEFAULT_REST_CONSTRAINT(void* r);
void __SET_INIT(__SET* set,void* rest,void* (*clone)(void*),int (*rest_constraint)(void*));
void __SET_ADD(void* newobj, __SET* set);
void __SET_ADD_INTERNAL(void* newobj, __SET* set);
void __SET_ITERATE(__SET* set,void (*iterate)(void**,void*),void* pars);
void __SET_FOREACH(void** ret,__SET* set);
int __SET_FIND(void** ret,__SET* set,int (*pred)(void**,void*),void** pars);

#endif
/* -eof- */
