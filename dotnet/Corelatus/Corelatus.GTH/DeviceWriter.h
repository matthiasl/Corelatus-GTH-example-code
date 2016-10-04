#pragma once
using namespace System;

namespace Corelatus{
	namespace GTH {
		public ref class DeviceWriter :IDisposable
		{
		private:
			int descriptor;
			int flags;
		public:
			int Write(array<Byte> ^buffer, int offset, int length);
			~DeviceWriter();
		internal:
			DeviceWriter(int descriptor, int flags);
		};
	}
}

