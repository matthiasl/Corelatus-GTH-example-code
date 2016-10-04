#include "stdafx.h"
#include "ToneHandler.h"

using namespace System::Runtime::InteropServices;
using namespace System::Reflection;
using namespace Corelatus::GTH;

void ToneHandler::native_handle(const char *name, const int length)
{
	ToneArgs^ args = gcnew ToneArgs(gcnew String(name));
	ToneDetected(this, args);
}

ToneHandler::ToneHandler(){

	Type^ type = this->GetType();
	MethodInfo^ mi = type->GetMethod("native_handle", BindingFlags::NonPublic | BindingFlags::Instance);
	del = Delegate::CreateDelegate(type, mi);
}

IntPtr ToneHandler::native_ptr(){
	return Marshal::GetFunctionPointerForDelegate(del);
}