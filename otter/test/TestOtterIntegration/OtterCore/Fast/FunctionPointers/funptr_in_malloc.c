#pragma expect_abandoned(out_of_bounds) /* __SYMBOLIC() % 4 may be negative */
#pragma no_other_abandoned

void __SYMBOLIC(void *);

typedef unsigned long size_t;
void * malloc(size_t size);
typedef void(*EventHandler)();

void Handler1()
{
	;
}

void Handler2()
{
	;
}

void Handler3()
{
	;
}

void Handler4()
{
	;
}

int main(void)
{
	EventHandler* events = (EventHandler*)malloc(4 * sizeof(EventHandler));

	events[0] = Handler1;
	events[1] = Handler2;
	events[2] = Handler3;
	events[3] = Handler4;

	int i;
	__SYMBOLIC(&i);
	events[i % 4]();

	return 0;
}
