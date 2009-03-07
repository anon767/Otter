#include<stdarg.h>
#include "abstractset.h"

// Make the compiler happy (these are not executed)

void __ASSERT(int a){};
void __ASSUME(int a){};

int AND(int a,int b){ return a&&b; }
int OR(int a,int b){ return a||b; }
int NOT(int a){ return !a; }
int __SYMBOLIC(int a){ return 0;}
int __TRUTH_VALUE(int a){ return 0;}
void __CLONE(void* a,void* b,int c){}

#define IMPLY(X,Y)	OR(NOT(X),(Y))

// symbolic string functions (temporary)
void* __PTR_SYMBOLIC(int n){
	char* p = malloc(n);
	for(int i=0;i<n;i++)
		p[i] = __SYMBOLIC(1);
	return p;
}

#define __SYMBOLIC_STR_LEN__   2
char* __SYMBOLIC_STR(){
	char* s = malloc(__SYMBOLIC_STR_LEN__+1);
	for(int i=0;i<__SYMBOLIC_STR_LEN__;i++){
		s[i] = __SYMBOLIC(1);
		__ASSUME(s[i]>32);
		__ASSUME(s[i]!='0');
		__ASSUME(s[i]!=',');
	}
	s[__SYMBOLIC_STR_LEN__] = '\0';
	return s;
}
int __STRING_EQUAL(char* a,char* b){
	int pred = 1;
	for(int i=0;i<__SYMBOLIC_STR_LEN__;i++)
		pred = AND(pred,a[i]==b[i]);
	return pred;
}

// aux functions
void** __ARG(int arg1,...){
	va_list ap;
	int i;

	void** ret = malloc((arg1+1)*sizeof(void*));
	va_start(ap,arg1);

	for(i=0;i<arg1;i++)
		ret[i] = va_arg(ap,void*);
	va_end(ap);
	return ret;
}

void __ASSUME_SIMPLIFY(int exp){
	if(__TRUTH_VALUE(exp)==0) // unknown
		__ASSUME(exp);
}

/******** SET IMPLEMENTATION ***********/
void __SET_INIT(__SET* set,void* rest,void* (*clone)(void*)){
	set->head[0] = 0;
	set->head[1] = 0;
	set->rest = rest;
	set->clone = clone;
}

void __SET_ADD(void* newobj, __SET* set){
	__SET_ELM* newelm = malloc(sizeof(__SET_ELM));
	newelm->elm = newobj;
	newelm->next = set->head[1];
	set->head[1] = newelm;
}
void __SET_ADD_INTERNAL(void* newobj, __SET* set){
	__SET_ELM* newelm = malloc(sizeof(__SET_ELM));
	newelm->elm = newobj;
	newelm->next = set->head[0];
	set->head[0] = newelm;
}

// Return a symbolic pointer that is equal to any one of the internal elements, or nothing
void* __SET_ABSTRACT_INTERNAL(__SET* set){
	void* abstract_nothing = malloc(1); // so that no other concrete pointers equal it
	void* r; // symbolic
	int formula = (r==abstract_nothing); // TODO: make sure it's not converted to any if-stmt

	__SET_ELM* cur;
	cur = set->head[0];
	while(cur!=0){
		formula = OR(formula,r==cur->elm);
		cur = cur->next;
	}
	return r;
}

void __SET_ITERATE(__SET* set,void (*iterate)(void**,void*),void* pars){
	__SET_ELM* cur;
	for(int i=0;i<2;i++){
		cur = set->head[i];
		while(cur!=0){
			iterate(pars,cur->elm);
			cur = cur->next;
		}
	}
	void* newobj = set->clone(set->rest);
	__SET_ADD_INTERNAL(newobj,set);
	iterate(pars,newobj);
}

void __SET_FOREACH(void** ret,__SET* set){
	__SET_ELM* cur;
	for(int i=0;i<2;i++){
		cur = set->head[i];
		while(cur!=0){
			int symbolic;
			if(symbolic) {
				*ret = cur->elm;
				return;
			}
			cur = cur->next;
		}
	}
	void* newobj = set->clone(set->rest);
	*ret = newobj;
	__SET_ADD_INTERNAL(newobj,set);
}

int __SET_FIND(void** ret,__SET* set,int (*pred)(void**,void*),void** pars){
	int nr = 0;
	__SET_ELM* cur;
	for(int i=0;i<2;i++){
		cur= set->head[i];
		while(cur!=0){
			if(pred(pars,cur->elm)) {
				nr++;
				*ret = cur->elm;
			}
			cur = cur->next;
		}
	}
	void* newobj = set->clone(set->rest);
	if(pred(pars,newobj)){
		nr++;
		*ret = newobj;
		__SET_ADD_INTERNAL(newobj,set);
	}// TODO: delete newobj will delete constraints that only associate with newobj.
	else
		__ASSUME_SIMPLIFY(NOT(pred(pars,set->rest))); 
	return nr;
}

int __SET_SIZE(__SET* set){
	// TODO: how to deduce the size?
	int size;
	return size;
}
int __SET_FALSE_PRED(void** pars,void* x){
	return 0;
}

void __SET_REMOVE(void** obj, __SET* set){
	__ASSERT(0);
}

