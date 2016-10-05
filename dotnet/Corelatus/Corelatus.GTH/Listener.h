#pragma once

#include "Corelatus.GTH.h"
#include "DeviceReader.h"
#include "DeviceWriter.h"
#include "IConnection.h"

namespace Corelatus{
	namespace GTH{
		public ref class Listener :IConnection, IDisposable
		{
		private:
			int descriptor;
			int port;
			Device^ device;
		public:
			virtual DeviceReader^ GetReader();
			virtual DeviceWriter^ GetWriter();
			virtual DeviceReader^ GetReader(int flags);
			virtual DeviceWriter^ GetWriter(int flags);
			virtual int NativeHandle();

			 property Device^ Owner{
				 Device^ get(){
					return device;
				}
			}

			 property int Port{
				int get(){
					return port;
				}
			}
		internal:
			Listener(int descriptor, int port, Device^ owner);
			~Listener();
		};
	}
}
