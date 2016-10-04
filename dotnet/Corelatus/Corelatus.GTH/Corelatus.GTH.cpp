// This is the main DLL file.

#include "Corelatus.GTH.h"
#include "DeviceReader.h"
#include "DeviceWriter.h"
#include "Socket.h"
#include "IConnection.h"
#include "Listener.h"
#include "DataConnection.h"

#define MAX_BUFFER 4096

using namespace Corelatus::GTH;


Device^ Device::Connect(String^ addr){
	return Connect(addr, false);
}

Device^ Device::Connect(String^ addr, bool verbose){

	if (!init){
		win32_specific_startup();
		init = true;
	}

	Device^ dev = gcnew Device();
	dev->ptr_addr = Marshal::StringToHGlobalAnsi(addr);
	dev->api = new GTH_api();

	int res = gth_connect(dev->api, (char*)dev->ptr_addr.ToPointer(), verbose);

	if (res != 0)
		throw gcnew CorelatusException("Failed to connect to device.");

	dev->connected = true;
	return dev;
}

bool Device::WaitForReboot(String^ addr){
	IntPtr ptr_addr;
	int res = -1;
	try{
		ptr_addr = Marshal::StringToHGlobalAnsi(addr);
		res = gth_wait_for_reboot((const char*)ptr_addr.ToPointer());
	}
	finally{
		if (ptr_addr != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_addr);
	}
	return res == 0;
}

void Device::SwitchTo(String^ addr, String^ system,bool verbose){
	IntPtr ptr_addr;
	IntPtr ptr_system;

	try{
		ptr_addr = Marshal::StringToHGlobalAnsi(addr);
		ptr_system = Marshal::StringToHGlobalAnsi(system);

		gth_switch_to((const char*)ptr_addr.ToPointer(), (const char*)ptr_system.ToPointer(),verbose);
	}
	finally{
		if (ptr_addr != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_addr);
		if (ptr_system != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_system);
	}
}

String^ Device::QueryResourceAttribute(String^ name, String^ key){

	IntPtr ptr_name;
	IntPtr ptr_key;
	char buffer[MAX_BUFFER];
	
	try{
		ptr_name = Marshal::StringToHGlobalAnsi(name);
		ptr_key = Marshal::StringToHGlobalAnsi(key);

		int res = gth_query_resource_attribute(api, (const char*)ptr_name.ToPointer(), (const char*)ptr_key.ToPointer(), buffer, MAX_BUFFER);
		if (res != 0)
			throw gcnew CorelatusException(String::Format("Failed to read attribute {0}.{1}", name, key));

		return gcnew String(buffer);
	}
	finally{
		if (ptr_name != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_name);
		if (ptr_key != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_key);
	}
}

bool Device::WaitForMessageEnded(String^ jobId){

	IntPtr ptr_jobId;
	int res = -1;
	try{
		ptr_jobId=Marshal::StringToHGlobalAnsi(jobId);
		res=gth_wait_for_message_ended(api, (const char*)ptr_jobId.ToPointer());
	}
	finally{
		if (ptr_jobId != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_jobId);
	}
	return res == 0;
}

bool Device::Delete(String^ jobId){

	IntPtr ptr_jobId;
	int res = -1;
	try{
		ptr_jobId = Marshal::StringToHGlobalAnsi(jobId);
		res=gth_delete(api, (const char*)ptr_jobId.ToPointer());
	}
	finally{
		if (ptr_jobId != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_jobId);
	}
	return res == 0;
}

bool Device::Disable(String^ resource){

	IntPtr ptr_resource;
	int res = -1;
	try{
		ptr_resource = Marshal::StringToHGlobalAnsi(resource);
		res=gth_disable(api, (const char*)ptr_resource.ToPointer());
	}
	finally{
		if (ptr_resource != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_resource);
	}
	return res == 0;
}

bool Device::Enable(String^ resource, IEnumerable<DeviceAttribute^> ^attributes){
	IntPtr ptr_resource;
	int res = -1;
	try{
		ptr_resource = Marshal::StringToHGlobalAnsi(resource);

		GTH_attribute* native_atts = 0;

		int count = attributes == nullptr ? 0 : Enumerable::Count(attributes);
		if (count > 0){
			native_atts = new GTH_attribute[count];

			int i = 0;
			for each (DeviceAttribute^ att in attributes)
			{
				native_atts[i] = att->ToNative();
				i++;
			}
		}
		res = gth_enable(api, (const char*)ptr_resource.ToPointer(), native_atts, count);
	}
	finally{
		if (ptr_resource != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_resource);
	}
	return res == 0;
}

bool Device::Install(String^ name, String^ type, array<Byte> ^data){
	IntPtr ptr_name;
	IntPtr ptr_type;
	IntPtr ptr_data;
	int res = -1;

	try{
		ptr_name = Marshal::StringToHGlobalAnsi(name);
		ptr_type = Marshal::StringToHGlobalAnsi(type);
		ptr_data = Marshal::AllocHGlobal(data->Length);
		Marshal::Copy(data, 0, ptr_data, data->Length);

		res = gth_install(api, (const char*)ptr_name.ToPointer(), (const char*)ptr_type.ToPointer(), (const char*)ptr_data.ToPointer(), data->Length);
	}
	finally{
		if (ptr_name != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_name);
		if (ptr_type != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_type);
		if (ptr_data != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_data);
	}
	return res == 0;
}

bool Device::Map(String^ resource, String^% name){
	IntPtr ptr_resource;
	char buff_name[MAX_BUFFER];
	
	try{
		ptr_resource = Marshal::StringToHGlobalAnsi(resource);
		int res = gth_map(api, (const char*)ptr_resource.ToPointer(), buff_name, MAX_BUFFER);
		
		if (res != 0){
			name = nullptr;
			return false;
		}

		name = gcnew String(buff_name);
		return true;
	}
	finally{
		if (ptr_resource != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_resource);
	}
	return false;
}

bool Device::Unmap(String^ resource){
	IntPtr ptr_resource;
	int res = -1;
	try{
		ptr_resource = Marshal::StringToHGlobalAnsi(resource);
		res = gth_unmap(api, (const char*)ptr_resource.ToPointer());
	}
	finally{
		if (ptr_resource != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_resource);
	}
	return res == 0;
}

bool Device::NewConnection(String^ srcSpan, int srcTs, String^dstSpan, int dstTs, String^% jobId){
	IntPtr ptr_srcSpan;
	IntPtr ptr_dstSpan;
	char buff_jobId[MAX_BUFFER];

	try{
		ptr_srcSpan = Marshal::StringToHGlobalAnsi(srcSpan);
		ptr_dstSpan = Marshal::StringToHGlobalAnsi(dstSpan);

		int res = gth_new_connection(api, (const char*)ptr_srcSpan.ToPointer(), srcTs, (const char*)ptr_dstSpan.ToPointer(), dstTs, buff_jobId);
		if (res != 0){
			jobId == nullptr;
			return false;
		}

		jobId = gcnew String(buff_jobId);
		return true;
	}
	finally{
		if (ptr_srcSpan != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_srcSpan);
		if (ptr_dstSpan != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_dstSpan);
	}
	return false;
}

void Device::Nop(){
	gth_nop(api);
}

bool Device::Set(String^ resource, IEnumerable<DeviceAttribute^> ^attributes){
	IntPtr ptr_resource;
	int res = -1;
	try{
		ptr_resource = Marshal::StringToHGlobalAnsi(resource);
		int count = Enumerable::Count(attributes);
		GTH_attribute* native_atts = new GTH_attribute[count];

		int i = 0;
		for each (DeviceAttribute^ att in attributes)
		{
			native_atts[i] = att->ToNative();
			i++;
		}

		res = gth_set(api, (const char*)ptr_resource.ToPointer(), native_atts, count);
	}
	finally{
		if (ptr_resource != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_resource);
	}
	return res == 0;
}

bool Device::Set(String^ resource, DeviceAttribute ^attribute){
	IntPtr ptr_resource;
	int res = -1;
	try{
		ptr_resource = Marshal::StringToHGlobalAnsi(resource);
		GTH_attribute native_att = attribute->ToNative();

		res = gth_set_single(api, (const char*)ptr_resource.ToPointer(), native_att.key,native_att.value);
	}
	finally{
		if (ptr_resource != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_resource);
	}
	return res == 0;
}

bool Device::Reset(String^ resource){
	IntPtr ptr_resource;
	int res = -1;
	try{
		ptr_resource = Marshal::StringToHGlobalAnsi(resource);
		res = gth_reset(api, (const char*)ptr_resource.ToPointer());
	}
	finally{
		if (ptr_resource != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_resource);
	}
	return res == 0;
}

String^ Device::MyIpAddress(){
	return gcnew String(gth_my_ip_address(api));
}

GTH_api* Device::NativeApi(){
	return api;
}

IConnection^ Device::Listen(){
	int port;
	int socket=gth_make_listen_socket(&port);

	return gcnew Listener(socket, port, this);
}

IConnection^ Device::ListenUdp(){
	int port;
	int socket = gth_make_udp_socket(&port);

	return gcnew Listener(socket, port, this);
}

IConnection^ Device::WaitForAccept(IConnection^ listener){
	
	int socket = gth_wait_for_accept(listener->NativeHandle());
	return gcnew DataConnection(socket);
}

bool Device::NewCasR2MfcDetector(int tag, String^ span, int timeslot, IConnection^ connection, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{
		Listener^ listener = (Listener^)connection;

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		int res = gth_new_cas_r2_mfc_detector(api, tag, (const char*)ptr_span.ToPointer(), timeslot, buff_jobId, api->my_ip, listener->Port);
		
		if (res != 0){
			jobId = nullptr;
			return false;
		}

		jobId = gcnew String(buff_jobId);
		return true;
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return false;
}

bool Device::NewCasR2LineSigMonitor(int tag, String^ span, int timeslot, IConnection^ connection, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{
		Listener^ listener = (Listener^)connection;

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		int res = gth_new_cas_r2_mfc_detector(api, tag, (const char*)ptr_span.ToPointer(), timeslot, buff_jobId, api->my_ip, listener->Port);

		if (res != 0){
			jobId = nullptr;
			return false;
		}

		jobId = gcnew String(buff_jobId);
		return true;
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return false;
}

bool Device::NewLapdLayer(int tag, String^ span, int timeslot, String^ side, int sapi, int tei, IConnection^ connection, String^% jobId){
	IntPtr ptr_span;
	IntPtr ptr_side;
	char buff_jobId[MAX_BUFFER];
	try{
		Listener^ listener = (Listener^)connection;

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		ptr_side = Marshal::StringToHGlobalAnsi(side);

		int res = gth_new_lapd_layer(api, tag, (const char*)ptr_span.ToPointer(), timeslot, (const char*)ptr_side.ToPointer(),sapi,tei, buff_jobId, api->my_ip, listener->Port);

		if (res != 0){
			jobId = nullptr;
			return false;
		}

		jobId = gcnew String(buff_jobId);
		return true;
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
		if (ptr_side != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_side);
	}
	return false;
}

bool Device::NewLapdMonitor(int tag, String^ span, int timeslot, IConnection^ connection, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{
		Listener^ listener = (Listener^)connection;

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		int res = gth_new_lapd_monitor(api, tag, (const char*)ptr_span.ToPointer(), timeslot, buff_jobId, api->my_ip, listener->Port);

		if (res != 0){
			jobId = nullptr;
			return false;
		}

		jobId = gcnew String(buff_jobId);
		return true;
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return false;
}

bool Device::NewMtp2Monitor(int tag, String^ span, array<int>^ timeslot, IConnection^ connection, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{
		Listener^ listener = (Listener^)connection;

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		pin_ptr<int> ptr_timeslot = &timeslot[0];

		int res = gth_new_mtp2_monitor(api, tag, (const char*)ptr_span.ToPointer(), ptr_timeslot, timeslot->Length, buff_jobId, api->my_ip, listener->Port);

		if (res != 0){
			jobId = nullptr;
			return false;
		}

		jobId = gcnew String(buff_jobId);
		return true;
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return false;
}

bool Device::NewMtp2Monitor(int tag, String^ span, array<int>^ timeslot, IConnection^ connection, IEnumerable<DeviceAttribute^> ^attributes, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{
		Listener^ listener = (Listener^)connection;

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		pin_ptr<int> ptr_timeslot = &timeslot[0];

		int count = Enumerable::Count(attributes);
		GTH_attribute* native_atts = new GTH_attribute[count];

		int i = 0;
		for each (DeviceAttribute^ att in attributes)
		{
			native_atts[i] = att->ToNative();
			i++;
		}

		int res = gth_new_mtp2_monitor_opt(api, tag, (const char*)ptr_span.ToPointer(), ptr_timeslot, timeslot->Length, buff_jobId, api->my_ip, listener->Port, native_atts, count);

		if (res != 0){
			jobId = nullptr;
			return false;
		}

		jobId = gcnew String(buff_jobId);
		return true;
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return false;
}

DeviceWriter^ Device::NewPlayer(String^ span, int timeslot, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		int res = gth_new_player(api,(const char*)ptr_span.ToPointer(), timeslot,buff_jobId);

		if (res < 0){
			jobId = nullptr;
			return nullptr;
		}

		jobId = gcnew String(buff_jobId);
		return gcnew DeviceWriter(res,0);
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return nullptr;
}

DeviceReader^ Device::NewRecorder(String^ span, int timeslot, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		int res = gth_new_recorder(api, (const char*)ptr_span.ToPointer(), timeslot, buff_jobId);

		if (res < 0){
			jobId = nullptr;
			return nullptr;
		}

		jobId = gcnew String(buff_jobId);
		return gcnew DeviceReader(res,0);
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return nullptr;
}

DeviceReader^ Device::NewRecorder(String^ span, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		int res = gth_new_wide_recorder(api, (const char*)ptr_span.ToPointer(), buff_jobId);

		if (res < 0){
			jobId = nullptr;
			return nullptr;
		}

		jobId = gcnew String(buff_jobId);
		return gcnew DeviceReader(res, 0);
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return nullptr;
}

DeviceReader^ Device::NewToneDetector(String^ span, int timeslot,ToneHandler^ handle, String^% jobId){
	IntPtr ptr_span;
	char buff_jobId[MAX_BUFFER];
	try{

		ptr_span = Marshal::StringToHGlobalAnsi(span);
		int res = gth_new_tone_detector(api, (const char*)ptr_span.ToPointer(), timeslot, buff_jobId, (GTH_tone_handler*) handle->native_ptr().ToPointer());

		if (res < 0){
			jobId = nullptr;
			return nullptr;
		}

		jobId = gcnew String(buff_jobId);
		return gcnew DeviceReader(res, 0);
	}
	finally{
		if (ptr_span != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_span);
	}
	return nullptr;
}

Device::~Device(){
	try{
		if (!connected)
			return;

		int res = gth_bye(api);
		if (res != 0)
			throw gcnew CorelatusException("Failed to close the device.");
	}
	finally{
		if (ptr_addr != IntPtr::Zero)
			Marshal::FreeHGlobal(ptr_addr);
	}
}