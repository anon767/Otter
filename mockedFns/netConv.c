#include <netinet/in.h> // This is to get the typedefs for uint32_t and uint16_t

#if __BYTE_ORDER == __LITTLE_ENDIAN
uint32_t htonl(uint32_t hostlong) {
	uint32_t retval;
	char *p_out = &retval, *p_in = &hostlong;
	p_out[0] = p_in[3];
	p_out[1] = p_in[2];
	p_out[2] = p_in[1];
	p_out[3] = p_in[0];
	return retval;
}

uint16_t htons(uint16_t hostshort) {
	uint16_t retval;
	char *p_out = &retval, *p_in = &hostshort;
	p_out[0] = p_in[1];
	p_out[1] = p_in[0];
	return retval;
}

uint32_t ntohl(uint32_t netlong) {
	uint32_t retval;
	char *p_out = &retval, *p_in = &netlong;
	p_out[0] = p_in[3];
	p_out[1] = p_in[2];
	p_out[2] = p_in[1];
	p_out[3] = p_in[0];
	return retval;
}

uint16_t ntohs(uint16_t netshort) {
	uint16_t retval;
	char *p_out = &retval, *p_in = &netshort;
	p_out[0] = p_in[1];
	p_out[1] = p_in[0];
	return retval;
}
#elif __BYTE_ORDER == __BIG_ENDIAN
uint32_t htonl(uint32_t hostlong) {
	return hostlong;
}

uint16_t htons(uint16_t hostshort) {
	return hostshort;
}

uint32_t ntohl(uint32_t netlong) {
	return netlong;
}

uint16_t ntohs(uint16_t netshort) {
	return netshort;
}
#else
#error unknown endianness
#endif
