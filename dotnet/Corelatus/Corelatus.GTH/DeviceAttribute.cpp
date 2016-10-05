#include "stdafx.h"
#include "DeviceAttribute.h"

using namespace Corelatus::GTH;
using namespace System::Runtime::InteropServices;

DeviceAttribute::DeviceAttribute(String^ key,String^ value)
{
	this->key = key;
	this->value = value;

	this->ptr_key = Marshal::StringToHGlobalAnsi(key);
	this->ptr_value = Marshal::StringToHGlobalAnsi(value);
}

DeviceAttribute::~DeviceAttribute(){
	if (ptr_key != IntPtr::Zero)
		Marshal::FreeHGlobal(ptr_key);

	if (ptr_value != IntPtr::Zero)
		Marshal::FreeHGlobal(ptr_value);
}

GTH_attribute DeviceAttribute::ToNative(){

	GTH_attribute att;
	att.key = (char*)ptr_key.ToPointer();
	att.value = (char*)ptr_value.ToPointer();
	return att;
}
