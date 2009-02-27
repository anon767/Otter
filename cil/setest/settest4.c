/******** SET IMPLEMENTATION ***********/

// temporary
void __SYMBOLIC_STRING(char** a){
	*a = malloc(1);
	**a = __SYMBOLIC();
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

void __SET_FOREACH(void** ret,__SET* set){
	__SET_ELM* cur = set->head;
	while(cur!=0){
		int symbolic;
		if(symbolic) {
			*ret = cur->elm;
			return;
		}
		cur = cur->next;
	}
	void* newobj = set->clone(set->rest);
	*ret = newobj;
	__SET_ADD(newobj,set);
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
		// Use NOT here because cil converts (!) into if statements
		__ASSUME(NOT(pred(set->rest))); 
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

//char* client_equal__target;
//int client_equal(void* ep){
//	return __STRING_EQUAL( ((CLIENT*)ep)->id, client_equal__target);
//}

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

char* channel_equal__target;
int channel_equal(void* ep){
	return __STRING_EQUAL( ((CHANNEL*)ep)->name, channel_equal__target);
}

void* channel_clone(void* src_void){
	CHANNEL* src = (CHANNEL*)src_void;
	CHANNEL* tar = make_symbolic_channel();
	__CLONE(&tar->data,&src->data,sizeof(int));
	__CLONE(tar->name,src->name,1); 
	// constraint: any channel has different name from the rest
	__ASSUME(NOT(__STRING_EQUAL(tar->name,src->name)));
	return tar;
}

/******** CL2CHAN ***********/
typedef struct _cl2chan{
	CLIENT* client;
	CHANNEL* channel;
} CL2CHAN;

CL2CHAN* make_symbolic_cl2chan(){
	CL2CHAN* c = malloc(sizeof(CL2CHAN));
	c->client = make_symbolic_client();
	c->channel = make_symbolic_channel();
	return c;
}

CL2CHAN* cl2chan_equal__target;
int cl2chan_equal(void* ep){
	CL2CHAN *p = (CL2CHAN*) ep;
	// physical equality
	return cl2chan_equal__target->client==p->client && cl2chan_equal__target->channel==p->channel; 
	//client_equal__target = cl2chan_equal__target->client->id;
	//channel_equal__target = cl2chan_equal__target->channel->name;
	//return AND(client_equal(p->client),channel_equal(p->channel));
}

void* cl2chan_clone(void* src_void){
	CL2CHAN* src = (CL2CHAN*)src_void;
	CL2CHAN* tar = malloc(sizeof(CL2CHAN));
	tar->client = src->client;
	tar->channel = src->channel;
	// don't really need to migrate the constraints since we use physical equality to distinguish elements
	// and physical inequality naturally holds for different objects due to our abstract set rep.
	//tar->client = client_clone(src->client);
	//tar->channel = channel_clone(src->channel); // not quite true: can have multiple copies of the same channel
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
	// running variables
	CLIENT* cl;
	CHANNEL* chan;
	CL2CHAN* cl2chan,*cl2chan2;

	// May not really need the set constraints...
	//__SET_CONSTRAIN(
	//		__FORALL(&chan,&My_Channels, 
	//		__FORALL(&b,&My_Channels, 
	//		__IMPLY((chan!=b),(!__STRING_EQUAL(chan->name,b->name)))
	//	)));

	
	// 1. if chan does not exist, create it.
	channel_equal__target = some_channel_name;
	num =  __SET_FIND(&chan,&My_Channels,channel_equal);

	if(num==0){
		chan = make_symbolic_channel();
		__ASSUME(__STRING_EQUAL(chan->name,some_channel_name));
		__SET_ADD(chan,&My_Channels);
	}
	__ASSERT(chan!=0);
	
	// 2. if client is already in the channel, return.
	cl2chan = malloc(sizeof(CL2CHAN));
	cl2chan->client = client;
	cl2chan->channel = chan;
	cl2chan_equal__target = cl2chan;
	num =  __SET_FIND(&cl2chan2,&My_Cl2Chan,cl2chan_equal);
	if(num>0) return 1;
	
	
	// 3. put client in the channel
	__SET_ADD(cl2chan,&My_Cl2Chan);

	cl2chan_equal__target = cl2chan;
	num =  __SET_FIND(&cl2chan2,&My_Cl2Chan,cl2chan_equal);
	__ASSERT(num==1);

	// 4. for all y in the channel s.t. y is not the client, tell y that client joins in.
	
	//__SET_FORALL(&cl,&My_Clients){
	//	printf("%s\n",chan->name);
	//}

	
	return 0;
}

int main(){
	// set up
	__SET_INIT(&My_Clients,make_symbolic_client(),client_clone);
	__SET_INIT(&My_Channels,make_symbolic_channel(),channel_clone);
	__SET_INIT(&My_Cl2Chan,make_symbolic_cl2chan(),cl2chan_clone);

	char* some_channel_name;
	__SYMBOLIC_STRING(&some_channel_name);

	CLIENT* client;
	__SET_FOREACH(&client,&My_Clients);

	IRC_JOIN(client,some_channel_name);

	return 0;
}


