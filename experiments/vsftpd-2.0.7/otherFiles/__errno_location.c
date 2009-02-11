int* __errno_location() {
	static int mock_errno;
	mock_errno = __SYMBOLIC(0);
	return &mock_errno;
}
