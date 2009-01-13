
/* khooyp: adapted from M. Zitser, R. Lippmann and T. Leek. "Testing Static Analysis Tools using
 * Exploitable Buffer Overflows from Open Source Code". ACM SIGSOFT Software Engineering Notes
 * (2004) vol. 29 (6).
 * also at http://samate.nist.gov/SRD/view_testcase.php?tID=1285
 */
#define STRSIZE 10

int main(void) {
    char src[STRSIZE], dst[STRSIZE];

	__ASSUME (1);

    int i;
    for (i = 0; i < STRSIZE && src[i] != '\0'; i++) {
		__ASSERT (i<STRSIZE);
        dst[i] = src[i];
    }
	__ASSERT (i<STRSIZE);
    dst[i] = '\0'; /* ERROR: buffer overflow */


    return 0;
}
