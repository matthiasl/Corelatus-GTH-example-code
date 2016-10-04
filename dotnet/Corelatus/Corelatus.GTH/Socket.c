#include "Socket.h"

#include <winsock.h>
#include "GTH.h"

#include <stdio.h>

static int max_arg(int a, int b);

int close(int handle){
	return closesocket(handle);
}

int snd(int handle, const char* data, int len, int flags){
	return send(handle, data, len, flags);
}

int rcv(int handle, char* data, int len, int flags){
	return recv(handle, data, len, flags);
}

static int
max_arg(int a, int b)
{
	return (a > b) ? a : b;
}

int wait_for_packet(void* ptr_api, int data_socket)
{
	GTH_api* api = (GTH_api*)ptr_api;

	fd_set fds;
	int result;
	int nfds = max_arg(api->fd, data_socket) + 1;

	FD_ZERO(&fds);

	for (;;)
	{
		struct timeval tv = { 1, 0 };
		FD_SET(api->fd, &fds);
		FD_SET(data_socket, &fds);

		result = select(nfds, &fds, 0, 0, &tv);

		if (result < 0)
		{
			return -1; //failed
		}

		if (result == 0)
		{
			return 0;
		}

		if (FD_ISSET(api->fd, &fds))
		{
			gth_nop(api);
		}

		if (FD_ISSET(data_socket, &fds))
		{
			return 1;
		}
	}
}