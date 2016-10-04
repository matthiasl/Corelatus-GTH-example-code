
#include "Socket.h"
#include "Listener.h"

using namespace Corelatus::GTH;

Listener::Listener(int descriptor, int port, Device^ owner){
	this->descriptor = descriptor;
	this->port = port;
	this->device = owner;
}
Listener::~Listener(){
	close(descriptor);
}

int Listener::NativeHandle(){
	return descriptor;
}

DeviceReader^ Listener::GetReader(int flags){
	return gcnew DeviceReader(descriptor, flags);
}

DeviceReader^ Listener::GetReader(){
	return GetReader(0);
}

DeviceWriter^ Listener::GetWriter(int flags){
	return gcnew DeviceWriter(descriptor, flags);
}

DeviceWriter^ Listener::GetWriter(){
	return GetWriter(0);
}
