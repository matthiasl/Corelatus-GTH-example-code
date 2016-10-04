
#include "Socket.h"
#include "DataConnection.h"

using namespace Corelatus::GTH;

DataConnection::DataConnection(int descriptor){
	this->descriptor = descriptor;
}

DataConnection::~DataConnection(){
	close(descriptor);
}

int DataConnection::NativeHandle(){
	return descriptor;
}

DeviceReader^ DataConnection::GetReader(int flags){
	return gcnew DeviceReader(descriptor, flags);
}

DeviceReader^ DataConnection::GetReader(){
	return GetReader(0);
}

DeviceWriter^ DataConnection::GetWriter(int flags){
	return gcnew DeviceWriter(descriptor, flags);
}

DeviceWriter^ DataConnection::GetWriter(){
	return GetWriter(0);
}
