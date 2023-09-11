#include "symtest.h"
#include "symtest_mock.h"

/*
 *	TODO: make stat return 0 only when path  = filename
 */

/* httpd.c entry functions */
extern void accept_request(int client);

/* test for existent file get with symbolic file name and file content */
#define SYM_TEST_GET_200_CMD			"GET "
#define SYM_TEST_GET_200_HTDIR			"htdocs"
#define SYM_TEST_GET_200_FILENAME_BUF	"/xxxx"
#define SYM_TEST_GET_200_DATA_BUF		"xxxxxx"
static void sym_test_get_200(void *param) {
    FILE file;
    int sock;
	char SYM_TEST_GET_200_FILENAME[]  = SYM_TEST_GET_200_FILENAME_BUF;
	char SYM_TEST_GET_200_REQUEST[]   = SYM_TEST_GET_200_CMD   SYM_TEST_GET_200_FILENAME_BUF " HTTP/1.0\r\n\r\n";
	char SYM_TEST_GET_200_LOCALFILE[] = SYM_TEST_GET_200_HTDIR SYM_TEST_GET_200_FILENAME_BUF;
	char SYM_TEST_GET_200_DATA[] =  SYM_TEST_GET_200_DATA_BUF;
	char SYM_TEST_GET_200_REPLY[] = "HTTP/1.0 200";

	int SYM_TEST_GET_200_CMD_len = sizeof(SYM_TEST_GET_200_CMD) - 1;
	int SYM_TEST_GET_200_HTDIR_len = sizeof(SYM_TEST_GET_200_HTDIR) - 1;

	/* set up symbolic data */
	int i;
	for(i=1;SYM_TEST_GET_200_FILENAME[i];i++){
		char symbol;
		__ASSUME(symbol>32,symbol<128,symbol!='?',symbol!='/');
		SYM_TEST_GET_200_FILENAME[i] = symbol;
	}
	for(i=0;SYM_TEST_GET_200_FILENAME[i];i++){
		SYM_TEST_GET_200_REQUEST[SYM_TEST_GET_200_CMD_len+i] = SYM_TEST_GET_200_FILENAME[i];
		SYM_TEST_GET_200_LOCALFILE[SYM_TEST_GET_200_HTDIR_len+i] = SYM_TEST_GET_200_FILENAME[i];
	}
	for(i=0;SYM_TEST_GET_200_DATA[i];i++){
		char symbol; 
		__ASSUME(symbol!='\0',symbol!='\n');
		SYM_TEST_GET_200_DATA[i] = symbol;
	}

    /* set up */
    mock_recv_buf = SYM_TEST_GET_200_REQUEST;
    mock_recv_len = sizeof(SYM_TEST_GET_200_REQUEST) - 1; /* == strlen() */
    mock_recv_ptr = mock_recv_buf;
	mock_stat_file = SYM_TEST_GET_200_LOCALFILE;
    mock_stat_buf.st_mode = 0;
    mock_fopen_file = &file;
    mock_fgets_buf = SYM_TEST_GET_200_DATA;
    mock_fgets_len = sizeof(SYM_TEST_GET_200_DATA) - 1; /* == strlen() */
    mock_fgets_ptr = mock_fgets_buf;
    mock_feof_EOF = 0;
    mock_send_ptr = mock_send_buf;

    SYMTEST_ASSUME_GT(&sock, 0);
    SYMTEST_ASSUME_LT(&sock, 65536);
    accept_request(sock);
    //SYMTEST_ASSERT_GT(mock_send_ptr - mock_send_buf, sizeof(SYM_TEST_GET_200_DATA) - 1);
    SYMTEST_ASSERT_STR_BEGINS(mock_send_buf, SYM_TEST_GET_200_REPLY);

	
	/* check if data in buf == DATA */
	char* data = mock_send_buf;
	while(data[0]!='\r'||data[1]!='\n'){
		while(data[0]!='\r'||data[1]!='\n') data++;
		data+=2;
	}
	data+=2;

	SYMTEST_ASSERT_TRUE(strcmp(data,SYM_TEST_GET_200_DATA)==0);
}

/* test for existant file get */
#define TEST_GET_200_FILENAME "/file200"
#define TEST_GET_200_REQUEST "GET " TEST_GET_200_FILENAME " HTTP/1.0\r\n\r\n"
#define TEST_GET_200_DATA "Hello World!"
#define TEST_GET_200_RESPONSE "HTTP/1.0 200"
static void test_get_200(void *param) {
    FILE file;
    int sock;
    /* set up */
    mock_recv_buf = TEST_GET_200_REQUEST;
    mock_recv_len = sizeof(TEST_GET_200_REQUEST) - 1; /* == strlen() */
    mock_recv_ptr = mock_recv_buf;
    mock_stat_file = "htdocs" TEST_GET_200_FILENAME;
    mock_stat_buf.st_mode = 0;
    mock_fopen_file = &file;
    mock_fgets_buf = TEST_GET_200_DATA;
    mock_fgets_len = sizeof(TEST_GET_200_DATA) - 1; /* == strlen() */
    mock_fgets_ptr = mock_fgets_buf;
    mock_feof_EOF = 0;
    mock_send_ptr = mock_send_buf;

    SYMTEST_ASSUME_GT(&sock, 0);
    SYMTEST_ASSUME_LT(&sock, 65536);
    accept_request(sock);
    SYMTEST_ASSERT_GT(mock_send_ptr - mock_send_buf, sizeof(TEST_GET_200_DATA) - 1);
    SYMTEST_ASSERT_STR_BEGINS(mock_send_buf, TEST_GET_200_RESPONSE);
}


/* test for non-existant file get */
#define TEST_GET_404_FILENAME "/file404"
#define TEST_GET_404_REQUEST "GET " TEST_GET_200_FILENAME " HTTP/1.0\r\n\r\n"
#define TEST_GET_404_RESPONSE "HTTP/1.0 404"
static void test_get_404(void *param) {
    int sock;
    /* set up */
    mock_recv_buf = TEST_GET_404_REQUEST;
    mock_recv_len = sizeof(TEST_GET_404_REQUEST) - 1; /* == strlen() */
    mock_recv_ptr = mock_recv_buf;
    mock_stat_file = "";
    mock_fopen_file = NULL;
    mock_fgets_len = 0;
    mock_feof_EOF = 0;
    mock_send_ptr = mock_send_buf;

    SYMTEST_ASSUME_GT(&sock, 0);
    SYMTEST_ASSUME_LT(&sock, 65536);
    accept_request(sock);
    SYMTEST_ASSERT_STR_BEGINS(mock_send_buf, TEST_GET_404_RESPONSE);
}

int main(void) {
    symtest_run_test(sym_test_get_200, NULL);
    //symtest_run_test(test_get_200, NULL);
    //symtest_run_test(test_get_404, NULL);
    return 0;
}
