
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
	EventHandler* events = (EventHandler*)malloc(2 * sizeof(EventHandler));

	events[0] = Handler1;
	events[1] = Handler2;
	events[__SYMBOLIC() % 2] = Handler3;
	events[__SYMBOLIC() % 2] = Handler4;

	events[__SYMBOLIC() % 2]();

	return 0;
}
