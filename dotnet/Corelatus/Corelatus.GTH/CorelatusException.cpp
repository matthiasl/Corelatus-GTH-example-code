#include "stdafx.h"
#include "CorelatusException.h"

using namespace Corelatus;

CorelatusException::CorelatusException(String^ message) :Exception(message)
{
}
