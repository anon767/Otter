#include <poll.h>
#include <string.h>
#include "iosim.h"

int poll_endtime;
int poll_end_with_signal = 0;
extern int NGIRCd_SignalQuit;

int poll(struct pollfd fds[], nfds_t nfds, int timeout){
	static int time = 0;
	int i;
	int c = 0;

	if(time>=poll_endtime){
		if(poll_end_with_signal) // Ctrl-C, quit properly
		{
			NGIRCd_SignalQuit = 1;
			return 0;
		}
		else // quit immediately
			exit(1010);
	}
	for(i=3;i<nfds;i++){ // fd 0,1,2 are stdio
		int flag = 0;
		// If the value of fd is less than 0, events shall be ignored, 
		// and revents shall be set to 0 in that entry on return from poll().
		
		fds[i].revents = 0;

		if(fds[i].fd<0){
			continue;
		}

		IOSIM_fd[fds[i].fd]->sym_file->stat.st_size 
			= IOSIM_fd[fds[i].fd]->sym_file->size[time];
		IOSIM_fd[fds[i].fd]->sym_fileout->stat.st_size 
			= IOSIM_fd[fds[i].fd]->sym_fileout->size[time];


		if(IOSIM_fd[fds[i].fd]->fd_type==IOSIM_FD_SSOCK){
			sym_file_stream_t* ssock = IOSIM_fd[fds[i].fd];
			if(ssock->offset < ssock->sym_file->stat.st_size){
				//__COMMENT("ssock->offset");
				//__EVAL(ssock->offset);
				//__COMMENT("ssock->sym_file->stat.st_size");
				//__EVAL(ssock->sym_file->stat.st_size);
				fds[i].revents |= POLLPRI;
				flag = 1;
			}
		}
		else{
			// readable?
			// if fds[i].fd has sth in the stream, set fds[i].revents |= POLLIN
			if(!IOSIM_eof(fds[i].fd)){
				fds[i].revents |= POLLIN;
				flag = 1;
			}
			// writable?
			// we assume every socket is writable. 
			// if fds[i].events | POLLOUT  (i.e., has sth to write), set fds[i].revents |= POLLOUT
			if(fds[i].events | POLLOUT){
				fds[i].revents |= POLLOUT;
				flag = 1;
			}
			// __EVAL(fds[i].fd);
			// __EVAL(fds[i].events);
			// __EVAL(fds[i].revents);
		}
		c += flag;
	}

	time++;
	return c;
}

int event_recv(int fd,char* s,int t){
	sym_file_t* file = IOSIM_fd[fd]->sym_file;
	strcat(file->contents,s);
	file->size[t] = strlen(file->contents);
	return 0;
}

int event_send(int fd,int t){
	sym_file_t* out = IOSIM_fd[fd]->sym_fileout;
	out->size[t] = 1;
	return 0;
}

extern int listen_queue[IOSIM_MAX_EVENTS];
extern int listen_queue_last;
extern int listen_queue_size[IOSIM_MAX_EVENTS];

int event_accept(int fd,int t){
	listen_queue[listen_queue_last++] = fd;
	listen_queue_size[t] = listen_queue_last;
	return 0;
}

int event_end(int t){
	poll_endtime = t;
	return 0;
}

int event_end_with_signal(int t){
	poll_endtime = t;
	poll_end_with_signal = 1;
	return 0;
}
