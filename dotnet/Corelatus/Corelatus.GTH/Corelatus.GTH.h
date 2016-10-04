// Corelatus.GTH.h

#pragma once

#include "GTH.h"
#include "CorelatusException.h"
#include "DeviceAttribute.h"
#include "IConnection.h"
#include "ToneHandler.h"

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Runtime::InteropServices;
using namespace System::Linq;

namespace Corelatus {
	namespace GTH{

		public ref class Device:IDisposable
		{
		private:
			IntPtr ptr_addr;
			GTH_api* api;
			bool connected;
			static bool init;
		public:
			static Device^ Connect(String^ addr);
			static Device^ Connect(String^ addr, bool verbose);
			static bool WaitForReboot(String^ addr);
			static void SwitchTo(String^ addr, String^ mode, bool verbose);

			String^ QueryResourceAttribute(String^ name, String^ key);
			bool WaitForMessageEnded(String^ jobId);
			bool Delete(String^ jobId);
			bool Disable(String^ resource);
			bool Enable(String^ resource, IEnumerable<DeviceAttribute^> ^attributes);
			bool Install(String^ name, String^ type, array<Byte> ^data);
			bool Map(String^ resource,[Out] String^% name);
			bool Unmap(String^ resource);

			void Nop();
			bool Set(String^ resource, IEnumerable<DeviceAttribute^> ^attributes);
			bool Set(String^ resource, DeviceAttribute ^attribute);
			bool Reset(String^ resource);
			String^ MyIpAddress();
			IConnection^ Listen();
			IConnection^ ListenUdp();
			IConnection^ WaitForAccept(IConnection^ listener);

			bool NewConnection(String^ srcSpan, int srcTs, String^ dstSpan, int dstTs, String^% jobId);
			bool NewCasR2MfcDetector(int tag, String^ span, int timeslot, IConnection^ listener, String^% jobId);
			bool NewCasR2LineSigMonitor(int tag, String^ span, int timeslot, IConnection^ listener, String^% jobId);
			bool NewLapdLayer(int tag, String^ span, int timeslot, String^ side,int sapi, int tei, IConnection^ listener, String^% jobId);
			bool NewLapdMonitor(int tag, String^ span, int timeslot, IConnection^ listener, String^% jobId);
			bool NewMtp2Monitor(int tag, String^ span, array<int>^ timeslot, IConnection^ listener, String^% jobId);
			bool NewMtp2Monitor(int tag, String^ span, array<int>^ timeslot, IConnection^ listener,IEnumerable<DeviceAttribute^> ^attributes, String^% jobId);

			DeviceWriter^ NewPlayer(String^ span, int timeslot, String^% jobId);
			DeviceReader^ NewRecorder(String^ span, int timeslot, String^% jobId);
			DeviceReader^ NewRecorder(String^ span, String^% jobId);
			DeviceReader^ NewToneDetector(String^ span, int timeslot, ToneHandler^ handle, String^% jobId);


			~Device();
		internal:
			GTH_api* NativeApi();
		};
	}
}
