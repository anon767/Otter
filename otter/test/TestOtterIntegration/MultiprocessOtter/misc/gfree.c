#pragma no_other_abandoned

int main() {
	char *p = __otter_multi_gmalloc(5);
	__otter_multi_gfree(p);
	return 0;
}
