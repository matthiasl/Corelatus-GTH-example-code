#pragma once

#include "DeviceReader.h"
#include "DeviceWriter.h"

namespace Corelatus{
	namespace GTH{
		public interface class IConnection
		{
		public:
			DeviceReader^ GetReader();
			DeviceWriter^ GetWriter();
			DeviceReader^ GetReader(int flags);
			DeviceWriter^ GetWriter(int flags);
			int NativeHandle();
		};
	}
}