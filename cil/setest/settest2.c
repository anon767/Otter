/******** START IMPLEMENTATION ***********/

// temporary
void __SYMBOLIC_STRING(char** a){
	*a = malloc(1);
	**a = __SYMBOLIC();
	//__SYMBOLIC(&(*a));   // fail to work
}
int __STRING_EQUAL(char* a,char* b){
	return a[0]==b[0];
}

typedef struct _set_elm{
	void* elm;
	struct _set_elm* next;
} __SET_ELM;

typedef struct _set{
	__SET_ELM* head;
	void* rest; 
	void* (*clone)(void*); // how to clone an object
} __SET;

void __SET_INIT(__SET* set,void* rest,void* (*clone)(void*)){
	set->head = 0;
	set->rest = rest;
	set->clone = clone;
}

void __SET_ADD(void* newobj, __SET* set){
	__SET_ELM* newelm = malloc(sizeof(__SET_ELM));
	newelm->elm = newobj;
	newelm->next = set->head;
	set->head = newelm;
}

int __SET_FIND(void** ret,__SET* set,int (*pred)(void*)){
	int nr = 0;
	__SET_ELM* cur = set->head;
	while(cur!=0){
		if(pred(cur->elm)) {
			nr++;
			*ret = cur->elm;
		}
		cur = cur->next;
	}
	void* newobj = set->clone(set->rest);
	if(pred(newobj)){
		nr++;
		*ret = newobj;
		__SET_ADD(newobj,set);
	}// TODO: delete newobj will delete constraints that only associate with newobj.
	else
		__ASSUME(!pred(set->rest)); // PROBLEM: cil converts not(!) into if statements
	return nr;
}


#define __SET_FOREACH(X,Y)	while(__SET_FOREACH_PARAMETERS((X),(Y)))

/******** END IMPLEMENTATION ***********/

typedef struct _channel{
	char* name;
	int data;
} CHANNEL;

CHANNEL* make_symbolic_channel(){
	CHANNEL* c = malloc(sizeof(CHANNEL));
	__SYMBOLIC_STRING(&c->name);
	c->data = __SYMBOLIC();
	return c;
}

__SET My_Channels;

char* channel_name_equal__target;
int channel_name_equal(void* ep){
	return __STRING_EQUAL( ((CHANNEL*)ep)->name, channel_name_equal__target);
}

void* channel_clone(void* src_void){
	CHANNEL* src = (CHANNEL*)src_void;
	CHANNEL* tar = make_symbolic_channel();
	__CLONE(&tar->data,&src->data,sizeof(int));
	__CLONE(tar->name,src->name,1); // 1 is temporary
	return tar;
}

int main(){

	CHANNEL* a;
	CHANNEL* b;
	int num;
	char* 	target_channel;
	__SYMBOLIC_STRING(&target_channel);


	__SET_INIT(&My_Channels,make_symbolic_channel(),channel_clone);
	//__SET_CONSTRAIN(
	//		__FORALL(&a,&My_Channels, 
	//		__FORALL(&b,&My_Channels, 
	//		__IMPLY((a!=b),(!__STRING_EQUAL(a->name,b->name)))
	//	)));

	channel_name_equal__target = target_channel;
	num = __SET_FIND(&a,&My_Channels,channel_name_equal);
	__ASSERT(num<=1);

	//if(num==0){
	//	CHANNEL* new_channel = make_symbolic_channel();
	//	__ASSUME(__STRING_EQUAL(new_channel->name,target_channel));
	//	__SET_ADD(new_channel,&My_Channels);
	//}

	//__SET_FOREACH(&a,&My_Channels){
	//	printf("%s\n",a->name);
	//}

	//channel_name_equal__target = target_channel;
	//num = __SET_FIND(&a,&My_Channels,channel_name_equal);
	//__ASSERT(num<=2);
	//__ASSERT(num==1);
	
	return 0;
}



