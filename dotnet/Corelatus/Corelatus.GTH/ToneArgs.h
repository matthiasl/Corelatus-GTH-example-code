#pragma once

using namespace System;

namespace Corelatus{
	namespace GTH{
		public ref class ToneArgs :EventArgs
		{
		private:
			String^ name;
		public:
			property String^ Name{
				String^ get(){
					return name;
				}
			}
			ToneArgs(String^ name);
		};
	}
}
