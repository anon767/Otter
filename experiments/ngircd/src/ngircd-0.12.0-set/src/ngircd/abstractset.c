#include<stdarg.h>

// Make the compiler happy (these are not executed)
//

void __ASSERT(int a){};
void __ASSUME(int a){};

int AND(int a,int b){ return a&&b; }
int OR(int a,int b){ return a||b; }
int NOT(int a){ return !a; }
int __SYMBOLIC(){ return 0;}
int __TRUTH_VALUE(int a){ return 0;}




/******* (temporary) ***/
void __SYMBOLIC_STRING(char** a){
	*a = malloc(1);
	**a = __SYMBOLIC();
}
int __STRING_EQUAL(char* a,char* b){
	return a[0]==b[0];
}
#define IMPLY(X,Y)	OR(NOT(X),(Y))

void** __ARG(int arg1,...){
	va_list ap;
	int i;

	void** ret = malloc(arg1*sizeof(void*));
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

int __SET_DEFAULT_REST_CONSTRAINT(void* r){ return 1;} // unconstrained

void __SET_INIT(__SET* set,void* rest,void* (*clone)(void*),int (*rest_constraint)(void*)){
	set->head[0] = 0;
	set->head[1] = 0;
	set->rest = rest;
	set->clone = clone;
	if(rest_constraint==0)
		set->rest_constraint = __SET_DEFAULT_REST_CONSTRAINT;
	else set->rest_constraint = rest_constraint;
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
	//if(__GIVEN(set->rest_constraint(newobj),pred(pars,newobj))){
	if(pred(pars,newobj)){
		nr++;
		*ret = newobj;
		__SET_ADD_INTERNAL(newobj,set);
	}// TODO: delete newobj will delete constraints that only associate with newobj.
	else
		__ASSUME_SIMPLIFY(NOT(pred(pars,set->rest))); 
	return nr;
}

/******** END SET IMPLEMENTATION ***********/
