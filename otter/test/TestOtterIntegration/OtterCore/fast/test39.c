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
	EventHandler* events = (EventHandler*)malloc(2 * sizeof(EventHandler));

	events[0] = Handler1;
	events[1] = Handler2;
	unsigned int i,j,k;
	__SYMBOLIC(&i); __ASSUME(i < 2);
	__SYMBOLIC(&j); __ASSUME(j < 2);
	__SYMBOLIC(&k); __ASSUME(k < 2);
	events[i] = Handler3;
	events[j] = Handler4;

	events[k]();

	return 0;
}
