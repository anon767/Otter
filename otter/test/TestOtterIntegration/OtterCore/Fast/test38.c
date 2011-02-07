#pragma no_other_abandoned

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
