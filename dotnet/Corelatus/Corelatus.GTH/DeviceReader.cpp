#include "DeviceReader.h"

#include "GTH.h"
#include "Socket.h"
#include "CorelatusException.h"

using namespace Corelatus::GTH;

DeviceReader::DeviceReader(int descriptor, int flags)
{
	this->descriptor = descriptor;
	this->flags = flags;
}

DeviceReader::~DeviceReader(){
	close(descriptor);
}

int DeviceReader::Read(array<Byte> ^buffer, int offset, int length){

	pin_ptr<Byte> ptr_buff = &buffer[offset];
	int res = rcv(descriptor, (char*)ptr_buff, length, flags);
	return res;
}

void DeviceReader::ReadExact(array<Byte> ^buffer, int offset, int length){

	pin_ptr<Byte> ptr_buff = &buffer[offset];
	size_t this_time;

	while (length > 0) {
		this_time = rcv(descriptor, (char*)ptr_buff, length, flags);
		if (this_time <= 0)
			throw gcnew CorelatusException("failed to read from device.");

		length -= this_time;
		ptr_buff+=this_time;
	}
}