/*
 * schedule2, v8
 *
 * The input is modified so that argv and fgets are replaced by fresh symbolic values.
 * The "ratio" argument, being a float, is set to 0.5 (TODO: give it a random value).
 *
 * In v8, the bounds checking of prio is commented out. Here, we turn it into an
 * assertion check (if (...) { __FAILURE(); }).
 */
#pragma max_abandoned(1)
#pragma max_nodes(10000)
#pragma expect_abandoned(failure_reached)

#include <stdio.h>

// BEGIN "schedule2.h"
#define MAXPRIO 3
#define MAXLOPRIO 2
#define BLOCKPRIO 0
#define CMDSIZE 20 /* size of command buffer */

/* Scheduling commands */
#define NEW_JOB 1
#define UPGRADE_PRIO 2
#define BLOCK 3
#define UNBLOCK 4
#define QUANTUM_EXPIRE 5
#define FINISH 6
#define FLUSH 7

/* stati */
#define OK 0
#define TRUE 1
#define FALSE 0
#define BADNOARGS -1 /* Wrong number of arguments */
#define BADARG -2    /* Bad argument (< 0) */
#define MALLOC_ERR -3
#define BADPRIO -4   /* priority < 0 or > MAXPRIO */
#define BADRATIO -5  /* ratio < 0 or > 1 */
#define NO_COMMAND -6 /* No such scheduling command */

int get_command(/* int *command, *prio, float *ratio */);
 	  /* Get command from stdin. Return 0 on EOF */
void exit_here(/* int status */); /* Exit program with abs(status) */

int enqueue(/* int prio, struct process * job */);
              /* Put job at end of queue & reschedule */
int new_job(/* int prio */);
              /* allocate process block & enqueue. Return status */
int schedule(/* int command, prio, float ratio */);
              /* Carry out command. Return status */
struct process * get_current();
              /* Get current job. Reschedule, if necessary */
int reschedule(/* int prio */);
                 /* If prio higher than current job, reschedule */

int put_end(/* int prio, struct process * process */);
       /* Put process at end of prio queue. Return status */
int get_process(/* int prio, float ratio, struct process ** job */);
                /* get job from prio queue at ratio position. Return status */

struct process
{
    unsigned int pid;
    int priority;
    struct process *next;
};
// END "schedule2.h"

// TODO: get rid of these definitions?
int abs(int i) {
    if (i >= 0) return i;
    else return -i;
}
void __FAILURE() {}
// END TODO


static struct process * current_job;
static int next_pid = 0;

int
enqueue(prio, new_process)
     int prio;
     struct process *new_process;
{
    int status;
    if(status = put_end(prio, new_process)) return(status); /* Error */
    return(reschedule(prio));
}

struct queue
{
    int length;
    struct process *head;
};

static struct queue prio_queue[MAXPRIO + 1]; /* blocked queue is [0] */



void main() /* n3, n2, n1 : # of processes at prio3 ... */
{
    /* BEGIN_OTTER */
    int argc = MAXPRIO + 1;
    int argv[MAXPRIO + 1];
    int i;
    /* END_OTTER */

    int command, prio;
    float ratio;
    int nprocs, status, pid;
    struct process *process;
    if(argc != MAXPRIO + 1) exit_here(BADNOARGS);
    /* BEGIN_OTTER */
    for(i = 1; i <= MAXPRIO; i++) {
        // Otter does not support __SYMBOLIC(&argv[i])
        int tmp;
        __SYMBOLIC(&tmp);
        argv[i] = tmp;
    }
    /* END_OTTER */
    for(prio = MAXPRIO; prio > 0; prio--)
    {
    /* BEGIN_OTTER */
	if((nprocs = (argv[MAXPRIO + 1 - prio])) < 0) exit_here(BADARG);
    /* END_OTTER */
	for(; nprocs > 0; nprocs--)
	{
	    if(status = new_job(prio)) exit_here(status);
	}
    }
    /* while there are commands, schedule it */
    while((status = get_command(&command, &prio, &ratio)) > 0)
    {
	schedule(command, prio, ratio);
    }
    if(status < 0) exit_here(status); /* Real bad error */
    exit_here(OK);
}

int
get_command(command, prio, ratio)
    int *command, *prio;
    float *ratio;
{
    // This function used to get input via fgets;
    // now changed to take symbolic integers.
    /* BEGIN_OTTER */
    int v_command = -1;
    int v_prio = -1;
    int v_ratio = -1.0;

    __SYMBOLIC(&v_command);

	switch(v_command)
	{
	  case NEW_JOB :
        __SYMBOLIC(&v_prio);
	    break;
	  case UNBLOCK :
        v_ratio = 0.5; // OTTER does not handle symbolic float
	    break;
	  case UPGRADE_PRIO :
        __SYMBOLIC(&v_prio);
        v_ratio = 0.5; // OTTER does not handle symbolic float
	    break;
	}
    *command = v_command;
    *prio = v_prio;
    *ratio = v_ratio;
	return(TRUE);
    /* END_OTTER */
}

void exit_here(status)
     int status;
{
    exit(abs(status));
}


int
new_job(prio) /* allocate new pid and process block. Stick at end */
     int prio;
{
    int pid, status = OK;
    struct process *new_process;
    pid = next_pid++;
    new_process = (struct process *) malloc(sizeof(struct process));
    if(!new_process) status = MALLOC_ERR;
    else
    {
	new_process->pid = pid;
	new_process->priority = prio;
	new_process->next = (struct process *) 0;
	status = enqueue(prio, new_process);
	if(status)
	{
	    free(new_process); /* Return process block */
	}
    }
    if(status) next_pid--; /* Unsuccess. Restore pid */
    return(status);
}

int upgrade_prio(prio, ratio) /* increment priority at ratio in queue */
     int prio;
     float ratio;
{
    int status;
    struct process * job;
    if(prio < 1 || prio > MAXLOPRIO) return(BADPRIO);
    if((status = get_process(prio, ratio, &job)) <= 0) return(status);
    /* We found a job in that queue. Upgrade it */
    job->priority = prio + 1;
    return(enqueue(prio + 1, job));
}

int
block() /* Put current job in blocked queue */
{
    struct process * job;
    job = get_current();
    if(job)
    {
	current_job = (struct process *)0; /* remove it */
	return(enqueue(BLOCKPRIO, job)); /* put into blocked queue */
    }
    return(OK);
}

int
unblock(ratio) /* Restore job @ ratio in blocked queue to its queue */
     float ratio;
{
    int status;
    struct process * job;
    if((status = get_process(BLOCKPRIO, ratio, &job)) <= 0) return(status);
    /* We found a blocked process. Put it where it belongs. */
    return(enqueue(job->priority, job));
}

int
quantum_expire() /* put current job at end of its queue */
{
    struct process * job;
    job = get_current();
    if(job)
    {
	current_job = (struct process *)0; /* remove it */
	return(enqueue(job->priority, job));
    }
    return(OK);
}

int
finish() /* Get current job, print it, and zap it. */
{
    struct process * job;
    job = get_current();
    if(job)
    {
	current_job = (struct process *)0;
	reschedule(0);
	//fprintf(stdout, " %d", job->pid);
	free(job);
	return(FALSE);
    }
    else return(TRUE);
}

int
flush() /* Get all jobs in priority queues & zap them */
{
    while(!finish());
    //fprintf(stdout, "\n");
    return(OK);
}

struct process *
get_current() /* If no current process, get it. Return it */
{
    int prio;
    if(!current_job)
    {
	for(prio = MAXPRIO; prio > 0; prio--)
	{ /* find head of highest queue with a process */
	    if(get_process(prio, 0.0, &current_job) > 0) break;
	}
    }
    return(current_job);
}

int
reschedule(prio) /* Put highest priority job into current_job */
     int prio;
{
    if(current_job && prio > current_job->priority)
    {
	put_end(current_job->priority, current_job);
	current_job = (struct process *)0;
    }
    get_current(); /* Reschedule */
    return(OK);
}

int
schedule(command, prio, ratio)
    int command, prio;
    float ratio;
{
    int status = OK;
    switch(command)
    {
      case NEW_JOB :
        status = new_job(prio);
	break;
      case QUANTUM_EXPIRE :
        status = quantum_expire();
	break;
      case UPGRADE_PRIO :
        status = upgrade_prio(prio, ratio);
	break;
      case BLOCK :
        status = block();
	break;
      case UNBLOCK :
        status = unblock(ratio);
	break;
      case FINISH :
        finish();
	//fprintf(stdout, "\n");
	break;
      case FLUSH :
        status = flush();
	break;
      default:
	status = NO_COMMAND;
    }
    return(status);
}




int
put_end(prio, process) /* Put process at end of queue */
     int prio;
     struct process *process;
{
    struct process **next;
    // OTTER: the line below is commented out in v8.
    // if(prio > MAXPRIO || prio < 0) return(BADPRIO); /* Somebody goofed */
    // Therefore, a bad prio will overflow prio_queue below
    if(prio > MAXPRIO || prio < 0) __FAILURE();
     /* find end of queue */
    for(next = &prio_queue[prio].head; *next; next = &(*next)->next);
    *next = process;
    prio_queue[prio].length++;
    return(OK);
}

int
get_process(prio, ratio, job)
     int prio;
     float ratio;
     struct process ** job;
{
    int length, index;
    struct process **next;
    if(prio > MAXPRIO || prio < 0) return(BADPRIO); /* Somebody goofed */
    if(ratio < 0.0 || ratio > 1.0) return(BADRATIO); /* Somebody else goofed */
    length = prio_queue[prio].length;
    index = ratio * length;
    index = index >= length ? length -1 : index; /* If ratio == 1.0 */
    for(next = &prio_queue[prio].head; index && *next; index--)
        next = &(*next)->next; /* Count up to it */
    *job = *next;
    if(*job)
    {
	*next = (*next)->next; /* Mend the chain */
	(*job)->next = (struct process *) 0; /* break this link */
	prio_queue[prio].length--;
	return(TRUE);
    }
    else return(FALSE);
}
