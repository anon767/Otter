int* __errno_location() {
	static int mock_errno;
	return &mock_errno;
}
