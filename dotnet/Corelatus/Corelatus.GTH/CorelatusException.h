#pragma once

using namespace System;

namespace Corelatus{
	public ref class CorelatusException :Exception
	{
	public:
		CorelatusException(String^ message);
	};
}
