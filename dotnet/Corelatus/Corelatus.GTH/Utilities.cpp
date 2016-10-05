#include "stdafx.h"
#include "Utilities.h"
#include "Socket.h"
#include "CorelatusException.h"

using namespace Corelatus::GTH;

bool Utilities::WaitForPacket(Device^ device, IConnection^ connection){
	int res = wait_for_packet(device->NativeApi(), connection->NativeHandle());
	if (res == -1)
		throw gcnew CorelatusException("waiting on packet failed, connection closed abruptly.");

	return res > 0;
}
