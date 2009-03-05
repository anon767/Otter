#include<stdarg.h>
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
	if(__GIVEN(set->rest_constraint(newobj),pred(pars,newobj))){
		nr++;
		*ret = newobj;
		__SET_ADD_INTERNAL(newobj,set);
	}// TODO: delete newobj will delete constraints that only associate with newobj.
	else
		__ASSUME_SIMPLIFY(NOT(pred(pars,set->rest))); 
	return nr;
}

//#define __SET_FOREACH(X,Y)	while(__SET_FOREACH_PARAMETERS((X),(Y)))

/******** END SET IMPLEMENTATION ***********/


/******** CLIENT ***********/
typedef struct _client{
	char* id; 
} CLIENT;

CLIENT* make_symbolic_client(){
	CLIENT* c = malloc(sizeof(CLIENT));
	__SYMBOLIC_STRING(&c->id);
	return c;
}

void* client_clone(void* src_void){
	CLIENT* src = (CLIENT*)src_void;
	CLIENT* tar = make_symbolic_client();
	__CLONE(tar->id,src->id,1);
	return tar;
}


/******** CHANNEL ***********/
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

int channel_equal(void** pars,void* ep){
	char* channel_equal__target = pars[0];
	return __STRING_EQUAL( ((CHANNEL*)ep)->name, channel_equal__target);
}

void* channel_clone(void* src_void){
	CHANNEL* src = (CHANNEL*)src_void;
	CHANNEL* tar = make_symbolic_channel();
	__CLONE(&tar->data,&src->data,sizeof(int));
	__CLONE(tar->name,src->name,1); 
	// constraint: any channel has different name from the rest
	__ASSUME_SIMPLIFY(NOT(__STRING_EQUAL(tar->name,src->name)));
	return tar;
}

/******** CL2CHAN ***********/
typedef struct _cl2chan{
	CLIENT* client;
	CHANNEL* channel;
} CL2CHAN;

CL2CHAN* make_symbolic_cl2chan(){
	CL2CHAN* c = malloc(sizeof(CL2CHAN));
	c->client = __SYMBOLIC();
	c->channel = __SYMBOLIC();
	return c;
}

CL2CHAN* make_cl2chan(CLIENT* cl,CHANNEL* ch){
	CL2CHAN* c = malloc(sizeof(CL2CHAN));
	c->client = cl;
	c->channel = ch;
	return c;
}


int cl2chan_equal(void** pars,void* ep){
	CL2CHAN* cl2chan_equal__target = pars[0];
	CL2CHAN *p = (CL2CHAN*) ep;
	return AND(cl2chan_equal__target->client==p->client, cl2chan_equal__target->channel==p->channel); 
}

__SET* cl2chan_rest_constraint_clientset;
__SET* cl2chan_rest_constraint_channelset;
int cl2chan_rest_constraint(void* rest_void){
	CL2CHAN* rest = rest_void;
	int client_formula = rest->client==make_symbolic_client(); // equals to nothing
	int channel_formula = rest->channel==make_symbolic_channel();

	// TODO: this function should not know the internal representation of set
	__SET_ELM* cur;
	
	cur = cl2chan_rest_constraint_clientset->head[0];
	while(cur!=0){
		CL2CHAN* elm = cur->elm;
		client_formula = OR(client_formula,elm->client==rest->client);
		cur = cur->next;
	}
	cur = cl2chan_rest_constraint_channelset->head[0];
	while(cur!=0){
		CL2CHAN* elm = cur->elm;
		channel_formula = OR(channel_formula,elm->channel==rest->channel);
		cur = cur->next;
	}
	return AND(client_formula,channel_formula);
}

void* cl2chan_clone(void* src_void){
	CL2CHAN* src = (CL2CHAN*)src_void;
	CL2CHAN* tar = malloc(sizeof(CL2CHAN));
	__CLONE(&tar->client,&src->client,sizeof(CLIENT*));
	__CLONE(&tar->channel,&src->channel,sizeof(CHANNEL*));
	__ASSUME_SIMPLIFY(OR(tar->client!=src->client,tar->channel!=src->channel));
	return tar;
}


// Sets (in ngIRCd)
__SET My_Channels;
__SET My_Clients;
__SET My_Cl2Chan;



int IRC_JOIN(CLIENT* client,char* some_channel_name){
	// JOIN:
	// 1. if chan does not exist, create it.
	// 2. for all y in the channel, tell y that client joins in.
	// 3. put client in the channel
	//
	// JOIN (as in ngIRCd):
	// 1. if chan does not exist, create it.
	// 2. if client is already in the channel, return.
	// 3. put client in the channel
	// 4. for all y in the channel s.t. y is not the client, tell y that client joins in.

	int num;
	CHANNEL* chan;
	CL2CHAN* cl2chan;

	// 1. if chan does not exist, create it.
	num =  __SET_FIND(&chan,&My_Channels,channel_equal,__ARG(1,some_channel_name));
	__ASSERT(num>=0,num<=1);
	
	if(num==0){
		chan = make_symbolic_channel();
		__ASSUME_SIMPLIFY(__STRING_EQUAL(chan->name,some_channel_name));
		__SET_ADD(chan,&My_Channels);
	}
	__ASSERT(chan!=0);
	
	// 2. if client is already in the channel, return.
	num =  __SET_FIND(&cl2chan,&My_Cl2Chan,cl2chan_equal,__ARG(1,make_cl2chan(client,chan)));
	__ASSERT(num>=0,num<=1);

	if(num>0) {
		return 1;
	}

	num =  __SET_FIND(&cl2chan,&My_Cl2Chan,cl2chan_equal,__ARG(1,make_cl2chan(client,chan)));
	__ASSERT(num==0);

	//// 3. put client in the channel
	__SET_ADD(make_cl2chan(client,chan),&My_Cl2Chan);

	num =  __SET_FIND(&cl2chan,&My_Cl2Chan,cl2chan_equal,__ARG(1,make_cl2chan(client,chan)));
	__ASSERT(num==1);

	// 4. for all y in the channel s.t. y is not the client, tell y that client joins in.
	
	//__SET_FORALL(&cl2chan2,&My_Cl2Chan){
	//	if(cl2chan2->channel==chan && cl2chan->client!=client){
	//		conn = Client_Conn(cl2chan->client);
	//		Conn_SetFlag(conn,SEND_TO_USER);
	//	}
	//}
	
	return 0;
}

int main(){
	// set up
	__SET_INIT(&My_Clients,make_symbolic_client(),client_clone,0);
	__SET_INIT(&My_Channels,make_symbolic_channel(),channel_clone,0);
	cl2chan_rest_constraint_clientset = &My_Clients;
	cl2chan_rest_constraint_channelset = &My_Channels;
	__SET_INIT(&My_Cl2Chan,make_symbolic_cl2chan(),cl2chan_clone,cl2chan_rest_constraint);

	char* some_channel_name;
	__SYMBOLIC_STRING(&some_channel_name);

	CLIENT* client;
	__SET_FOREACH(&client,&My_Clients);

	IRC_JOIN(client,some_channel_name);

	return 0;
}


