#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <ctype.h>

#include <stdio.h>


int     socket(int domain, int type, int protocol){
	return 42;
}

int     bind(int socket, const struct sockaddr *address,
		             socklen_t address_len){
	return 0;
}

int     getsockname(int socket, struct sockaddr *address,
		             socklen_t *address_len){
	return 0;
}

int     listen(int socket, int backlog){
	return 0;
}

int     accept(int socket, struct sockaddr *address,
		             socklen_t *address_len){
	return 84;
}

char stream[] = 
	"GET ?\n" 
	"Remaining\n";
int index = 0;
int inited = 0;
char the_path;
void init(){
	char symb;
	__ASSUME(symb>='a',symb<='z');
	stream[4] = symb;
	the_path = symb;
}

ssize_t recv(int socket, void *buffer, size_t length, int flags){
	if(!inited){
		inited = 1;
		init();
	}
	int i;
	for(i=0;i<length;i++)
	{
		((char*)buffer)[i] = stream[index];
		if(stream[index]=='\0'){
			return i;
		}
		index++;
	}
	return length;
}

int    stat(const char * path, struct stat * st){
	char str[] = "htdocs?";
	str[6] =  the_path;
	if(strcmp(path,str)==0) // found
	{
		st->st_mode = S_IFREG;
		return 0;
	}
	else
	{
		return -1;
	}
}

FILE *fopen(const char *restrict filename, const char *restrict mode)
{
	return 226; //sth
}

ssize_t send(int socket, const void *message, size_t length, int flags)
{
	// TODO: check message contents
	return 0;
}
