#pragma once
using namespace System;

namespace Corelatus{
	namespace GTH {
		public ref class DeviceReader:IDisposable
	{
	private:
		int descriptor;
		int flags;
	public:
		int Read(array<Byte> ^buffer, int offset, int length);
		void ReadExact(array<Byte> ^buffer, int offset, int length);
		~DeviceReader();
	internal:
		DeviceReader(int descriptor, int flags);
	};
}
}

