int* __errno_location() {
	static int mock_errno;
	static char need_to_initialize_mock_errno = 1;
	if (need_to_initialize_mock_errno) {
		need_to_initialize_mock_errno = 0;
		mock_errno = __SYMBOLIC();
	}
	return &mock_errno;
}
