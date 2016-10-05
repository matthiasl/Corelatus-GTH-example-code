#pragma once

#include "Corelatus.GTH.h"
#include "IConnection.h"

using namespace System::Runtime::CompilerServices;

namespace Corelatus{
	namespace GTH{
		[ExtensionAttribute]
		public ref class Utilities abstract sealed
		{
		public:
			[ExtensionAttribute]
			static bool WaitForPacket(Device^ device, IConnection^ connection);
		};
	}
}
