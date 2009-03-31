#define main main_ngircd
#include "ngircd-0.12.0-set.c"
#undef main


REQUEST* make_join_request(){
	REQUEST *request = malloc(sizeof(REQUEST));
	request->prefix = "";
	request->command = "JOIN";
	request->argv[0] = malloc(10);
	__SYMBOLIC_STRING(request->argv[0]);
	request->argc = 1;
	return request;
}

int main(){
	/*
	 *	TODO:
	 *	To prove that at the end, Ch' = Ch U {x},
	 *	We first put some fresh y in Ch, 
	 *	and we also clone y to y'
	 *	then we check at the end, that y is still in Ch'
	 *	and y'===y.
	 *	That is, y in Ch -> y in Ch'.
	 *	And we further see if x is in Ch'.
	 *	At the same time, at the beginning we assume that z is not in Ch
	 *	Then we check if z is still not in Ch'.
	 *
	 *	To prove that after removing channel, Ch' = Ch \ {x},
	 *	
	 *
	 *	Consider really add FIND(x) before ADD(x) and REMOVE(x)
	 */
	__SYMBOLIC_STATE();
	Conf_MaxJoins = 0;
	Conf_PredefChannelsOnly = 0;

	CLIENT *client;
	//   char existing_channel_name[10];		__SYMBOLIC_STRING(existing_channel_name);
	//   char nonexisting_channel_name[10];	__SYMBOLIC_STRING(nonexisting_channel_name);
	/* initialization */
	Client_Init();
	Channel_Init();

	//   CHANNEL *existing_channel = Channel_Search(existing_channel_name);
	//   CHANNEL *nonexisting_channel = Channel_Search(nonexisting_channel_name);
	//   if(NOT(AND(existing_channel!=0,nonexisting_channel==0))) exit(1); 


	__SET_FOREACH(&client,&My_Clients);

	REQUEST *request = make_join_request();

	// Channels: C
	IRC_JOIN(client,request);
	// Channels: C+ c(request->argv[0]) if it's valid
	
	//   // existing channels still exist
	//   __ASSERT(Channel_Search(existing_channel_name)!=0);
	//   // nonexisting channels, if not equal to the requested channel, still don't exist
	//   __ASSERT(OR(__STRING_EQUAL(nonexisting_channel_name,request->argv[0]),
	//   			Channel_Search(nonexisting_channel_name)==0)); 
	
	//    // if the requested channel has valid name
	//    if(Channel_IsValidName(request->argv[0])){
	//    	CHANNEL *Chan = Channel_Search(request->argv[0]);
	//    	// Requested channel is in the channel set
	//    	__ASSERT(Chan!=0);
	//    	// client is in the channel
	//    	__ASSERT(Get_Cl2Chan(Chan,client)!=0);

	//    	CLIENT *client2;
	//    	// for each client2 in the client set, 
	//    	// if it's in the channel and not == client,
	//    	__SET_FOREACH(&client2,&My_Clients);
	//    	if(AND(client2!=client,Get_Cl2Chan(Chan,client2)!=0)){
	//    		// TODO: client2 receives ":{client->ID} JOIN :{request->argv[0]}"
	//    		__COMMENT("Expect: client2 receives \":{client->ID} JOIN :{request->argv[0]}\"");
	//    		__EVALSTR(client2->messages,strlen(client2->messages));
	//    	}
	//    }
	//    else{
	//    	// TODO: client receives ERR_NOSUCHCHANNEL
	//    	__COMMENT("Expect: ERR_NOSUCHCHANNEL");
	//    	__EVALSTR(client->messages,strlen(client->messages));
	//    }
	//    
	
	__COMMENT("DONE");

	return 0;
}

