#pragma once

#include "GTH.h"
using namespace System;

namespace Corelatus{
	namespace GTH{
		public ref class DeviceAttribute :IDisposable
		{
		private:
			String^ key;
			String^ value;
			IntPtr ptr_key;
			IntPtr ptr_value;
		public:
			property String^ Key{
				String^ get(){
					return key;
				}
			}
			property String^ Value{
				String^ get(){
					return value;
				}
			}
			DeviceAttribute(String^ key, String^ value);
			~DeviceAttribute();
		internal:
			GTH_attribute ToNative();
		};
	}
}
