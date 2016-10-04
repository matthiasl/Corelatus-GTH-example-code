#pragma once

int close(int handle);
int snd(int handle, const char* data, int len, int flags);
int rcv(int handle, char* data, int len, int flags);
int wait_for_packet(void *api, int data_socket);