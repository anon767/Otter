#pragma expect_return(__return_code__ == 0)
#pragma expect_return(__return_code__ == 1)
#pragma no_other_abandoned

int main(char** argc, int argv)
{
	if(__otter_multi_fork())
		return 0;
	else
		return 1;
}
