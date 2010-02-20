#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[])
{
 int sockfd;
 int len;
 struct sockaddr_in address;
 int result;
 char ch = 'A';
 char* msg = argv[2];
 char buf[1024];

 sockfd = socket(AF_INET, SOCK_STREAM, 0);
 address.sin_family = AF_INET;
 address.sin_addr.s_addr = inet_addr("127.0.0.1");
 address.sin_port = htons(atoi(argv[1]));
 len = sizeof(address);
 result = connect(sockfd, (struct sockaddr *)&address, len);

 if (result == -1)
 {
  perror("oops: client1");
  exit(1);
 }
 sprintf(buf,"%s\n",msg);
 printf("string to server = %s\n",buf);
 write(sockfd, buf, strlen(buf));
 sprintf(buf,"\n");
 printf("string to server = %s\n",buf);
 write(sockfd, buf, strlen(buf));

 read(sockfd, buf, 1024);
 printf("string from server = %s\n", buf);
 close(sockfd);
 exit(0);
}
