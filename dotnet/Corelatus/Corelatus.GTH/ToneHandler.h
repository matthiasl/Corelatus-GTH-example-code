#pragma once

#include "ToneArgs.h"

using namespace System;

namespace Corelatus{
	namespace GTH{
		public ref class ToneHandler
		{
		private:
			Delegate^ del;
		public:
			event EventHandler<ToneArgs^>^ ToneDetected;
			ToneHandler();

		internal:
			void native_handle(const char *name, const int length);
			IntPtr native_ptr();
		};
	}
}
