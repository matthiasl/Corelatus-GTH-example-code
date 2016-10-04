#pragma once

#include "Corelatus.GTH.h"
#include "DeviceReader.h"
#include "DeviceWriter.h"
#include "IConnection.h"

namespace Corelatus{
	namespace GTH{
		public ref class DataConnection :IConnection, IDisposable
		{
		private:
			int descriptor;
		public:
			virtual DeviceReader^ GetReader();
			virtual DeviceWriter^ GetWriter();
			virtual DeviceReader^ GetReader(int flags);
			virtual DeviceWriter^ GetWriter(int flags);
			virtual int NativeHandle();

		internal:
			DataConnection(int descriptor);
			~DataConnection();
		};
	}
}
