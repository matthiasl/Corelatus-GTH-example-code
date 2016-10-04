#include "DeviceWriter.h"

#include "Socket.h"

using namespace Corelatus::GTH;

DeviceWriter::DeviceWriter(int descriptor, int flags)
{
	this->descriptor = descriptor;
	this->flags = flags;
}

DeviceWriter::~DeviceWriter(){
	close(descriptor);
}

int DeviceWriter::Write(array<Byte> ^buffer, int offset, int length){

	pin_ptr<Byte> ptr_buff = &buffer[offset];
	int res = snd(descriptor, (const char*)ptr_buff, length, flags);
	return res;
}