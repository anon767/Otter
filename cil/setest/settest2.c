
typedef struct _channel{
	char* name;
	int data;
} CHANNEL;

CHANNEL* make_symbolic_channel(){
	CHANNEL* c = malloc(sizeof(CHANNEL));
	__SYMBOLIC_STRING(&c->name);
	__SYMBOLIC(&c->data);
	return c;
}


_SET* My_Channels;

int main(){

	char* 	target_channel;
	__SYMBOLIC_STRING(&target_channel);

	// "rest" of channels in My_Channels
	CHANNEL* a;
	CHANNEL* b;
	CHANNEL* rest_of_my_channels = make_symbolic_channel();

	_SET_INIT(My_Channels,rest_of_my_channels);
	_SET_CONSTRAIN(My_Channels, 
			IMPLY(	
				AND(
					(a==_SET_ANYELEMENT(My_Channels)), 
					(b==_SET_ANYELEMENT(My_Channels)),
					(a!=b)
				),
				(_STREQ(a->name,b->name)==FALSE)
			)
		);

	_SET* My_Channels_named_target;
	_SET_INIT(My_Channels_named_target,NULL);
	_SET_ITERATE(My_Channels;a){
		// Expectation: the execution will branch

		if(_STREQ(a->name,target_channel)==TRUE){
			// the set has target_channel
			_SET_ADD(My_Channels_named_target,a);
		}
		else{
			// the set does not have target_channel
		}
	}

	int size = _SET_SIZE(My_Channels_named_target);
	__ASSERT(size<=1);

	if(size==0){
		CHANNEL* new_channel = make_symbolic_channel();
		__ASSUME(_STREQ(new_channel->name,target_channel)==TRUE);
		_SET_ADD(My_Channels,new_channel);
	}

	_SET_INIT(My_Channels_named_target,NULL);
	_SET_ITERATE(My_Channels;a){
		// Expectation: the execution will branch

		if(_STREQ(a->name,target_channel)==TRUE){
			// the set has target_channel
			_SET_ADD(My_Channels_named_target,a);
		}
		else{
			// the set does not have target_channel
		}
	}
	int size = _SET_SIZE(My_Channels_named_target);
	__ASSERT(size==1);
	
	return 0;
}



