
/* khooyp: adapted from http://samate.nist.gov/SRD/view_testcase.php?tID=89 */

int main(void) {
    char src[] = "a string"; /* assert(sizeof(src) == 9) */
    char dst[8];             /* assert(sizeof(dst) == 8) */
    char *str, *p;

    int i;
    str = src;
	p = dst;
	str = src;
	while((*p++ = *str++)) /* ERROR: buffer overflow */
	{
		continue;
	}
	__ASSERT(AND(p-dst<=sizeof(dst),str-src<=sizeof(src)));

    return 0;
}
