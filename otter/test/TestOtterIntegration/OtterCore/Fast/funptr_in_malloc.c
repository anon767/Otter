#pragma expect_abandoned(out_of_bounds) /* __SYMBOLIC() % 4 may be negative */
#pragma no_other_abandoned

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

int main(char** argc, int argv)
{
	EventHandler* events = (EventHandler*)malloc(4 * sizeof(EventHandler));

	events[0] = Handler1;
	events[1] = Handler2;
	events[2] = Handler3;
	events[3] = Handler4;

	events[__SYMBOLIC() % 4]();

	return 0;
}
