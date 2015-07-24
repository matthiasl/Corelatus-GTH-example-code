//----------------------------------------------------------------------
// An example program. Shows how to capture SS7 data off the wire (an E1/T1)
// with a GTH and save it to a file (or stdout) in either PCap-NG or
// classic PCap format for further analysis with e.g. wireshark or tshark.
//
// References:
//
//  PCap-NG doc: http://www.winpcap.org/ntar/draft/PCAP-DumpFileFormat.html
// classic PCap: http://wiki.wireshark.org/Development/LibpcapFileFormat
//      GTH API: http://www.corelatus.com/gth/api/
//
// Limitations:
//
//  Doesn't attempt any API error handling.
//  Doesn't check or keep track of L1 status
//  Doesn't log L1 or L2 errors and state changes
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2009, Corelatus AB Stockholm
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in the
//       documentation and/or other materials provided with the distribution.
//     * Neither the name of Corelatus nor the
//       names of its contributors may be used to endorse or promote products
//       derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY Corelatus ''AS IS'' AND ANY
// EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL Corelatus BE LIABLE FOR ANY
// DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//----------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <assert.h>
#include <string.h>
#include <signal.h>
#include <time.h>

#ifdef WIN32
#include <winsock2.h>
#include <windows.h>
#else
#include <sys/socket.h>
#include <sys/select.h>
#include <arpa/inet.h>
#endif // WIN32

#include "gth_win32_compat.h"
#include "gth_apilib.h"

// Types used for fixed-length packets. These typedefs are checked at runtime.
typedef unsigned int u32;
typedef unsigned short u16;

//--------------------------------------------------
// PCap classic file format structures.
#pragma pack(1)
typedef struct {
	u32 magic;
	u16 major_version;
	u16 minor_version;
	u32 GMT_to_localtime;
	u32 sigfigs;
	u32 snaplen;
	u32 network;
} PACK_SUFFIX PCap_classic_global_header;

#pragma pack(1)
typedef struct {
	u32 ts_sec;
	u32 ts_us;
	u32 incl_len;
	u32 orig_len;
} PACK_SUFFIX PCap_classic_packet_header;

//--------------------------------------------------
// Pcap-NG file format structures

#pragma pack(1)
typedef struct {
	u16 code;
	u16 length;
} PACK_SUFFIX PCap_NG_option;


#pragma pack(1)
typedef struct {
	u32 type;
	u32 block_total_length;
	u32 byte_order_magic;
	u16 major_version;
	u16 minor_version;
	unsigned long long section_length;
} PACK_SUFFIX PCap_NG_shb;

typedef struct {
	u32 type;
	u32 block_total_length;
	u16 link_type;
	u16 reserved;
	u32 snaplen;
} PACK_SUFFIX PCap_NG_idb;

#pragma pack(1)
typedef struct {
	u32 type;
	u32 block_total_length;
	u32 interface_id;
	u32 timestamp_hi;
	u32 timestamp_lo;
	u32 captured_len;
	u32 packet_len;
} PACK_SUFFIX PCap_NG_epb;

// GTH socket structure. An SS7 MTP-2 signal unit can't be more than
// 279 octets long according to Q.703.
#define MAX_SIGNAL_UNIT 300

// There are 31 useable timeslots in an E1
#define MAX_TIMESLOTS 31

// GTH 2.x has 16 E1/T1 inputs
// RAN probe has 64
#define MAX_SPANS 64

// How many MTP-2 channels can we run at the same time? (Also limited by
// hardware)
#define MAX_MTP2_CHANNELS 72

typedef struct {
	u16 tag;
	u16 flags;
	u16 timestamp_hi;
	u32 timestamp_lo;
	char payload[MAX_SIGNAL_UNIT];
} GTH_mtp2;

enum PCap_format { PCAP_CLASSIC, PCAP_NG };

// Link types, defined in PCap-NG spec appendix C
#define LINK_TYPE_MTP2 140

typedef struct {
	char *span;
	int timeslots[MAX_TIMESLOTS];
	int n_timeslots;
} Channel_t;

//----------------------------------------------------------------------
void
usage() {
	fprintf(stderr,
		"save_to_pcap git_head: %s build_hostname: %s\n\n"

		"save_to_pcap <options> <GTH-IP> <channels> [<channels>...] <filename>"
		"\n\nSave decoded MTP-2 signal units to a file in libpcap format, "
		"\nsuitable for examining with wireshark, tshark or other network"
		"\nanalyser software.\n"
		"\n<options>: [-c] [-f fisu=no] [-m] [-n <rotation>] [-v]"
		"\n-c: save in the classic Pcap format (default is the newer Pcap-NG)"
		"\n-f fisu=no: remove all MTP-2 FISUs"
		"\n-f esnf=yes: use MTP-2 extended sequence numbers"
		"\n-m: tells the GTH that you are using a -20dB monitor point"
		"\n-n <packets:c>: rotate the output file after <c> packets"
		"\n-n <duration:s>: rotate the output file after <s> seconds"
		"\n-s Stop the process instead of rotating the files"
		"\n-v: print API commands and responses (verbose)"
		"\n"
		"\n<GTH-IP> is the GTH's IP address or hostname"
		"\n<channels> is a list of spans and timeslots:"
		"\n  <span> [<span>...] <timeslot> [<timeslot>...]"
		"\n  e.g. 1A 2A 1 2 3 monitors timeslots 1, 2 and 3 on span 1A and 2A."
		"\n  e.g. 1A 1 2A 2 3A 3 4 monitors timeslot 1 on 1A, 2 on 2A and 3 and 4 on 3A."
		"\n<channels> can also be a range of timeslots for Nx64kbit/s signalling):"
		"\n  <span> <timeslot>-<timeslot>[,<timeslot>-<timeslot>]"
		"\n  e.g. 1A 1-31 monitors one 1980kbit/s channel on timeslots 1-31"
		"\n  e.g. 1A 1-15,17-31 monitors one 1920kbit/s channel"
		"\n  e.g. 1A 1-4 1B 1-4 monitors two 256kbit/s channels"
		"\n<span> is the name of a span, e.g. '1A'"
		"\n<timeslot> is a timeslot number, from 1 to 31"
		"\n<filename> can be -, which means standard output.\n\n",
		git_head, build_hostname);

	fprintf(stderr,
		"Examples (on hardware with electrical E1/T1 ports):\n"
		"./save_to_pcap 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
		"./save_to_pcap 172.16.1.10 1A 2A 3A 4A 1 2 3 4 isup_capture.pcapng\n"
		"./save_to_pcap 172.16.1.10 1A 2A 16 1B 5 6 7 8 capture.pcapng\n"
		"./save_to_pcap -m 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
		"./save_to_pcap -m -n packets:1000 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
		"./save_to_pcap -m -n duration:60 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
		"./save_to_pcap -c 172.16.1.10 1A 2A 16 - | tshark -V -i - \n"
		"./save_to_pcap -c 172.16.1.10 1A 2A 16 - | wireshark -k -i - \n"
		"./save_to_pcap -c 172.16.1.10 1A 2A 16 \\\\.\\pipe\\isup_capture.1\n");

	fprintf(stderr,
		"\nExamples (on SDH/SONET hardware, usually optical):\n"
		"./save_to_pcap 172.16.1.10 pcm55 16 isup_capture.pcapng\n\n");

	exit(-1);
}

//----------------------------------------------------------------------
// File IO functions which have to be different on Unix and Win32.
//
// (Win32 has fwrite, fclose and friends, but they can't be used to write
// to a named pipe, hence these wrappers).
#ifdef WIN32
// Return 1 on success
#define fwrite write_to_handle_or_file
static int
write_to_handle_or_file(void *buffer,
int length,
int items,
HANDLE_OR_FILEPTR file)
{
	int result;
	DWORD written;

	result = WriteFile(file, buffer, length * items, &written, 0);
	if (written != length || !result) {
		return 0;
	}
	return 1;
}

static HANDLE_OR_FILEPTR
stdout_handle_or_file()
{
	return GetStdHandle(STD_OUTPUT_HANDLE);
}

#define fclose CloseHandle

#else
static HANDLE_OR_FILEPTR
stdout_handle_or_file()
{
	return stdout;
}

#endif

static HANDLE_OR_FILEPTR
open_windows_pipe(const char *filename)
{
#ifdef WIN32
	HANDLE pipe;
	int result;

	pipe = CreateNamedPipe(filename,
		PIPE_ACCESS_OUTBOUND,          // write-only
		PIPE_TYPE_MESSAGE | PIPE_WAIT, // blocking writes
		1,                             // only allow one pipe
		70000,          // write buffer. GTH max is 64k
		10000,          // read buffer. We don't read
		0,              // default timeout
		0);             // no security attributes

	if (pipe == INVALID_HANDLE_VALUE) {
		die("Unable to create a named pipe. Giving up.");
	}

	result = ConnectNamedPipe(pipe, 0);
	if (!result) {
		die("Unabled to connect the named pipe. Giving up.");
	}

	return pipe;
#else
	die("Cannot open a windows named pipe on a non-windows OS. Giving up.");
	return 0;
#endif
}

static void
flush_file(HANDLE_OR_FILEPTR hf)
{
#ifdef WIN32
	FlushFileBuffers(hf);
#else
	fflush(hf);
#endif
}

static void
open_file_for_writing(HANDLE_OR_FILEPTR *hf, const char *filename)
{
	int result = 0;
#ifdef WIN32
	*hf = CreateFile(filename,
		GENERIC_WRITE,
		0,
		0,
		CREATE_ALWAYS,
		FILE_ATTRIBUTE_NORMAL,
		0);

	if (*hf == INVALID_HANDLE_VALUE) {
		*hf = 0;
	}
#else
	result = fopen_s(hf, filename, "wb");
#endif
	if (*hf == 0 || result != 0) {
		fprintf(stderr, "unable to open %s for writing. Aborting.\n", filename);
		exit(-1);
	}
}

//----------------------------------------------------------------------

// Optical E1/T1: just enable; no special options required.
static void
enable_optical_l1(GTH_api *api,
const char* span,
const int monitoring)
{
	int result;

	if (monitoring)
		fprintf(stderr, "Warning: ignoring -m switch on optical hardware.\n");

	result = gth_enable(api, span, 0, 0);

	if (result != 0)
		die("Setting up L1 failed. (-v switch gives more information)");
}

// Electrical E1: disable TX pins and possibly enable -20dB monitoring
static void
enable_electrical_l1(GTH_api *api,
const char* span,
const int monitoring)
{
	int result;
	char span_name[20];

	int n_attributes = (monitoring) ? 3 : 2;
	GTH_attribute attributes[] = { { "status", "enabled" },
	{ "tx_enabled", "false" },
	{ "monitoring", "true" }
	};

	assert(sizeof(span_name) > (strlen(span) + strlen("pcm")));
	strncpy_s(span_name, sizeof span_name, "pcm", sizeof span_name - 1);
	strncat(span_name, span, sizeof span_name);

	// Use <set> here: <enable> isn't supported until gth2_system_37a.
	result = gth_set(api, span_name, attributes, n_attributes);

	if (result != 0)
		die("Setting up L1 failed. (-v switch gives more information)");
}

// Start L1 for all channels.
// This can cause an L1 to be started multiple times. That's OK.
static void
enable_l1(GTH_api *api,
const Channel_t channels[],
const int n_channels,
const int monitoring)
{
	char architecture[10];
	int i;
	int result;

	result = gth_query_resource_attribute(api, "board", "architecture",
		architecture, 10);
	if (result != 0)
		die("Unable to query hardware architecture. Giving up.");

	architecture[3] = 0;
	if (strcmp(architecture, "gth") == 0) {
		for (i = 0; i < n_channels; i++) {
			enable_electrical_l1(api, channels[i].span, monitoring);
		}
	}
	else {
		for (i = 0; i < n_channels; i++) {
			enable_optical_l1(api, channels[i].span, monitoring);
		}
	}
}

#define MAX_MTP2_ATTRS 2

// Start up MTP-2 monitoring on the given span and timeslot
static void
monitor_mtp2(GTH_api *api,
const Channel_t *channel,
int tag,
int drop_fisus,
int esnf,
int listen_port,
int listen_socket
)
{
	int result;
	char job_id[MAX_JOB_ID];
	GTH_attribute attrs[MAX_MTP2_ATTRS];
	int n_attrs = 0;

	if (drop_fisus) {
		attrs[n_attrs].key = "fisu";
		attrs[n_attrs].value = "no";
		n_attrs++;
	}

	if (esnf) {
		attrs[n_attrs].key = "esnf";
		attrs[n_attrs].value = "yes";
		n_attrs++;
	}

	result = gth_new_mtp2_monitor_opt(api, tag,
		channel->span,
		channel->timeslots,
		channel->n_timeslots,
		job_id, api->my_ip, listen_port,
		attrs, n_attrs);
	if (result != 0)
		die("Setting up MTP2 monitoring failed. (-v gives more information)");

	return;
}

// Read exactly the requested number of bytes from the given descriptor
void
read_exact(int fd, char* buf, size_t count)
{
	size_t this_time;

	while (count > 0) {
		this_time = recv(fd, buf, count, 0);
		if (this_time <= 0)
			die("MTP-2 data socket from GTH unexpectedly closed\n");

		count -= this_time;
		buf += this_time;
	}
}

void inline
checked_fwrite(void *b, int n, HANDLE_OR_FILEPTR f)
{
	int result = fwrite(b, n, 1, f);

	if (result != 1) {
		die("fwrite failed. (Is the output file writeable?)");
	}
}


static void
write_pcap_classic_header(HANDLE_OR_FILEPTR file)
{
	PCap_classic_global_header header;

	// The pcap file is native-endian, i.e. wireshark uses the magic value
	// to figure out if it was created on a little-endian or big-endian machine.
	header.magic = 0xa1b2c3d4;
	header.major_version = 2;
	header.minor_version = 4;
	header.GMT_to_localtime = 0;
	header.sigfigs = 0;
	header.snaplen = 65535;
	header.network = LINK_TYPE_MTP2;

	checked_fwrite((void*)&header, sizeof header, file);

	return;
}

// Must be a multiple of 4 because of padding rules in PCap-ng
#define MAX_HW_DESCRIPTION 400
char hw_description[MAX_HW_DESCRIPTION];

void
read_hw_description(GTH_api *api, const char *hostname)
{
	int result;
	char architecture[10];

	result = gth_query_resource_attribute(api, "board", "architecture",
		architecture, 10);
	if (result != 0)
		die("Unable to query hardware architecture. Giving up.");

	result = snprintf(hw_description, MAX_HW_DESCRIPTION,
		"save_to_pcap (Corelatus %s %s)",
		architecture, hostname);
	if (result >= MAX_HW_DESCRIPTION)
		die("Hardware description is too long");
}

static int inline
round_up_32_bit(int x)
{
	return (x + 3) & (~3);
}

static void
write_pcap_ng_shb(HANDLE_OR_FILEPTR file)
{
	PCap_NG_shb shb;
	u32 btl;
	PCap_NG_option userappl = { 4, 0 };        // 4 = user application name
	PCap_NG_option end_of_options = { 0, 0 };
	userappl.length = strlen(hw_description);

	btl = sizeof(PCap_NG_shb)
		+ sizeof(userappl) + round_up_32_bit(userappl.length)
		+ sizeof(end_of_options)
		+ sizeof(shb.block_total_length);

	shb.type = 0x0A0D0D0A;
	shb.block_total_length = btl;
	shb.byte_order_magic = 0x1A2B3C4D;
	shb.major_version = 1;
	shb.minor_version = 0;
	shb.section_length = 0xffffffffFFFFFFFFULL; // "unknown"

	checked_fwrite((void*)&shb, sizeof shb, file);
	checked_fwrite((void*)&userappl, sizeof userappl, file);
	checked_fwrite((void*)hw_description, round_up_32_bit(userappl.length), file);
	checked_fwrite((void*)&end_of_options, sizeof end_of_options, file);
	checked_fwrite((void*)&btl, sizeof(btl), file);

	return;
}

// Must be a multiple of 4 because of padding rules in PCap-ng
#define MAX_IF_NAME 32

// Having both if_description and if_name causes Wireshark 1.10.3 to
// display only if_description. So we only have if_name.
static void
write_pcap_idbs(HANDLE_OR_FILEPTR file, Channel_t *c, int n)
{
	int x;
	PCap_NG_option end_of_options = { 0, 0 };

	for (x = 0; x < n; x++) {
		PCap_NG_idb idb;
		PCap_NG_option if_name;
		char idb_if_name[MAX_IF_NAME];
		PCap_NG_option if_tsresol;
		char tsresol[4] = { 3, 0, 0, 0 }; // 10^-3 resolution, three padding bytes

		u32 block_total_length;

		if_name.code = 2;    // if_name
		if_name.length = snprintf(idb_if_name, MAX_IF_NAME, "%s:%d",
			c[x].span, c[x].timeslots[0]);
		if (if_name.length < 0 || if_name.length >= MAX_IF_NAME)
			die("interface name is too long");

		block_total_length = sizeof(idb)
			+ sizeof(PCap_NG_option) + round_up_32_bit(if_name.length)
			+ sizeof(PCap_NG_option) + sizeof(tsresol)
			+ sizeof(PCap_NG_option)
			+ sizeof(block_total_length);

		idb.type = 1;
		idb.block_total_length = block_total_length;
		idb.link_type = LINK_TYPE_MTP2;
		idb.reserved = 0;
		idb.snaplen = 279;

		if_tsresol.code = 9; // tsresol
		if_tsresol.length = 1;

		checked_fwrite((void*)&idb, sizeof(idb), file);

		checked_fwrite((void*)&if_name, sizeof(if_name), file);
		checked_fwrite((void*)&idb_if_name, round_up_32_bit(if_name.length), file);

		checked_fwrite((void*)&if_tsresol, sizeof(if_tsresol), file);
		checked_fwrite((void*)&tsresol, sizeof(tsresol), file);

		checked_fwrite((void*)&end_of_options, sizeof(end_of_options), file);

		checked_fwrite((void*)&block_total_length, sizeof block_total_length, file);
	}
}

static void
write_pcap_global_header(HANDLE_OR_FILEPTR file,
enum PCap_format format,
	Channel_t *channels,
	int n_channels)
{
	switch (format) {
	case PCAP_CLASSIC:
		write_pcap_classic_header(file);
		break;

	case PCAP_NG:
		write_pcap_ng_shb(file);
		write_pcap_idbs(file, channels, n_channels);
		break;

	default: die("internal error writing global pcap header");
	}
}

static void write_classic_packet_header(HANDLE_OR_FILEPTR file,
	u32 timestamp_hi,
	u32 timestamp_lo,
	int length)
{
	PCap_classic_packet_header pcap_header;
	unsigned long long ts_sec;
	unsigned long long ts_us;
	int result;

	assert(sizeof ts_sec == 8);

	ts_us = timestamp_hi;
	ts_us <<= 32;
	ts_us += timestamp_lo;

	ts_sec = ts_us / 1000;
	ts_us = (ts_us % 1000) * 1000;

	pcap_header.ts_sec = (u32)ts_sec;
	pcap_header.ts_us = (u32)ts_us;
	pcap_header.incl_len = length;
	pcap_header.orig_len = length;

	result = fwrite(&pcap_header, sizeof pcap_header, 1, file);
	if (result != 1) {
		die("Unable to write packet to the given file. (Is it writeable?)");
	}
}

static u32
write_ng_packet_header(HANDLE_OR_FILEPTR file,
u32 timestamp_hi,
u32 timestamp_lo,
u16 tag,
int length)
{
	PCap_NG_epb epb;

	epb.type = 6;
	epb.block_total_length = sizeof(epb)
		+ round_up_32_bit(length)
		+ sizeof(epb.block_total_length);
	epb.interface_id = tag;
	epb.timestamp_hi = timestamp_hi;
	epb.timestamp_lo = timestamp_lo;
	epb.captured_len = length;
	epb.packet_len = length;

	checked_fwrite(&epb, sizeof epb, file);

	return epb.block_total_length;
}

static inline void
write_packet(HANDLE_OR_FILEPTR file,
u32 timestamp_hi,
u32 timestamp_lo,
u16 tag,
void *payload,
int length,
int format)
{
	u32 total_length;

	switch (format) {
	case PCAP_CLASSIC:
		write_classic_packet_header(file, timestamp_hi, timestamp_lo, length);
		checked_fwrite(payload, length, file);
		break;

	case PCAP_NG:
		total_length = write_ng_packet_header(file, timestamp_hi,
			timestamp_lo, tag, length);
		checked_fwrite(payload, round_up_32_bit(length), file);
		checked_fwrite(&total_length, sizeof total_length, file);
		break;

	default:
		die("internal error");
	}
}

static int
is_filename_a_pipe(const char *name)
{
	const char pipe_prefix[] = "\\\\.\\pipe\\";
	return (strncmp(pipe_prefix, name, strlen(pipe_prefix)) == 0);
}

// windows.h has a max() function. On *nix, sys/param.h has a MAX macro
// We write our own to avoid confusion.
static int
max_arg(int a, int b)
{
	return (a > b) ? a : b;
}



// Block (or loop) until a packet arrives. Flush API events with <nop/>
// Returns 0 on timeout, 1 otherwise.
static int
wait_for_packet(GTH_api *api, int data_socket)
{
	fd_set fds;
	int result;
	int nfds = max_arg(api->fd, data_socket) + 1;

	FD_ZERO(&fds);

	for (;;)
	{
		struct timeval tv = { 1, 0 };
		FD_SET(api->fd, &fds);
		FD_SET(data_socket, &fds);

		result = select(nfds, &fds, 0, 0, &tv);

		if (result < 0)
		{
			die("internal error---select() returned error");
		}

		if (result == 0)
		{
			return 0;
		}

		if (FD_ISSET(api->fd, &fds))
		{
			gth_nop(api);
		}

		if (FD_ISSET(data_socket, &fds))
		{
			return 1;
		}
	}
}

// Timers are totally different on Windows and the Unixes. We just
// write the code twice to handle that.
#ifdef WIN32
static HANDLE timer;

static void
set_timer(int seconds)
{
	LARGE_INTEGER liDueTime;
	int result;

	liDueTime.QuadPart = seconds;
	// Timer is in 100ns units. Negative means relative timeout.
	liDueTime.QuadPart *= -10000000LL;

	result = SetWaitableTimer(timer, &liDueTime, 0, NULL, NULL, 0);
	if (!result) die("SetWaitableTimer failed");
}

static void
init_timer(int seconds)
{
	timer = CreateWaitableTimer(NULL, TRUE, "file_rotation_timer");
	if (timer == NULL) die("CreateWaitableTimer failed");
	set_timer(seconds);
}

// Return: 0 if the timer has expired
static int
read_and_restart_timer(int seconds)
{

	if (WaitForSingleObject(timer, 0) == WAIT_OBJECT_0)
	{
		set_timer(seconds);
		return 0;
	}
	return 1;
}


#else   // unix version
static timer_t timer;

static void
set_timer(int seconds)
{
	int result;
	struct itimerspec new_timer = { { 0, 0 }, { seconds, 0 } };

	result = timer_settime(timer, 0, &new_timer, 0);
	if (result != 0) die("timer_settime failed");
}

static void
init_timer(int seconds)
{
	int result;
	struct sigevent se;

	se.sigev_notify = SIGEV_NONE;

	result = timer_create(CLOCK_REALTIME, &se, &timer);
	if (result != 0) die("unable to create a timer");

	set_timer(seconds);
}

// Return: 0 if the timer has expired
static int
read_and_restart_timer(int seconds)
{
	int result;
	struct itimerspec current;

	result = timer_gettime(timer, &current);
	if (result != 0) die("timer_gettime() failed");

	if (current.it_value.tv_sec == 0 && current.it_value.tv_nsec == 0)
	{
		set_timer(seconds);
		return 0;
	}

	return 1;
}
#endif

static inline int
is_time_to_rotate(int su_count, int n_sus_per_file, int duration)
{
	if (duration > 0 && read_and_restart_timer(duration) == 0)
	{
		return 1;
	}
	return (n_sus_per_file > 0 && (su_count >= n_sus_per_file));
}

#define MAX_FILENAME 100

// Loop forever, converting the incoming GTH data to libpcap format
static void
convert_to_pcap(GTH_api *api,
int data_socket,
const char *base_name,
const int n_sus_per_file,
const int duration_per_file,
const int stop_after_interval,
Channel_t channels[],
int n_channels,
const enum PCap_format format)
{
	u16 length;
	GTH_mtp2 signal_unit;
	int su_count;
	int file_number = 1;
	HANDLE_OR_FILEPTR file;
	int write_to_stdout = 0;
	int write_to_pipe;

	write_to_stdout = (strcmp(base_name, "-") == 0);
	write_to_pipe = is_filename_a_pipe(base_name);

	init_timer(duration_per_file);

	int always_true = 1;
	time_t rawtime;
	struct tm * timeinfo;

	while (always_true) {
		char filename[MAX_FILENAME];

		time(&rawtime);
		timeinfo = localtime(&rawtime);
		if (!write_to_stdout && !write_to_pipe)
		{
			if (!stop_after_interval)
			{
				snprintf(filename, MAX_FILENAME, "%s_%05d_%04d%02d%02d%02d%02d%02d",
					base_name, file_number, timeinfo->tm_year + 1900, timeinfo->tm_mon + 1,
					timeinfo->tm_mday, timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
			}
			else
			{
				snprintf(filename, MAX_FILENAME, "%s",
					base_name);

			}
			open_file_for_writing(&file, filename);
			fprintf(stderr, "saving to file %s\n", filename);
		}
		else if (write_to_stdout)
		{
			file = stdout_handle_or_file();
			fprintf(stderr, "saving capture to stdout\n");
		}
		else
		{
			fprintf(stderr, "saving capture to a windows named pipe\n");
			file = open_windows_pipe(base_name);
		}

		write_pcap_global_header(file, format, channels, n_channels);

		file_number++;
		su_count = 0;

		int rotation_time_reached = 0;
		do
		{
			if (wait_for_packet(api, data_socket) != 0)
			{
				read_exact(data_socket, (void*)&length, sizeof length);
				length = ntohs(length);
				assert(length <= sizeof signal_unit);
				read_exact(data_socket, (void*)&signal_unit, length);

				length -= (signal_unit.payload - (char*)&(signal_unit.tag));
				write_packet(file,
					ntohs(signal_unit.timestamp_hi),
					ntohl(signal_unit.timestamp_lo),
					ntohs(signal_unit.tag),
					signal_unit.payload,
					length,
					format);
				flush_file(file);
				su_count++;
			}
		} while (!(rotation_time_reached = is_time_to_rotate(su_count, n_sus_per_file, duration_per_file))
			|| write_to_pipe
			|| write_to_stdout);

		if (rotation_time_reached && stop_after_interval)
		{
			fprintf(stderr, "Stopped capturing when rotation time reached\n");
			always_true = 0;
		}
		fclose(file);
	}
}

static int
is_span_name(char *arg)
{
	if (strstr(arg, "pcm")) return 1;  // Optical hardware uses e.g. 'pcm55'
	if (strstr(arg, "A")) return 1;
	if (strstr(arg, "B")) return 1;
	if (strstr(arg, "C")) return 1;
	if (strstr(arg, "D")) return 1;
	return 0;
}

static void
print_channels(Channel_t *channels, int n)
{
	int x;
	int y;

	for (x = 0; x < n; x++) {
		fprintf(stderr, "monitoring %s:", channels->span);
		for (y = 0; y < channels->n_timeslots - 1; y++) {
			fprintf(stderr, "%d,", channels->timeslots[y]);
		}
		fprintf(stderr, "%d ", channels->timeslots[y]);
		fprintf(stderr, "interface_id=%d\n", x);
		channels++;
	}
}

// Examples of arguments "16", "1-3", "1-3,5-9,10", "1,2,3"
//
// We handle this by first breaking the string at every comma, then
// expanding out minus-sign ranges.
#define MAX_RANGE 6    // widest possible range: "11-12"
static void
argument_to_ts_array(const char* s, int timeslots[], int *n_timeslots)
{
	char ranges[MAX_TIMESLOTS][MAX_RANGE];  // Longest-possible
	int range = 0;
	char *pos;
	int i;

	*n_timeslots = 0;

	while ((pos = strchr(s, ',')))
	{
		strncpy_s(ranges[range++], MAX_RANGE, s, pos - s);
		s = pos + 1;
		if (range > MAX_TIMESLOTS)
			die("Too many timeslots specified. Abort.");
	}
	strncpy_s(ranges[range++], MAX_RANGE, s, strlen(s));

	for (i = 0; i < range; i++)
	{
		int lo;
		int hi;
		int result;

		result = sscanf(ranges[i], "%d-%d", &lo, &hi);

		if (result == 0)
		{
			fprintf(stderr, "Got %s", ranges[i]);
			die(" when expecting a timeslot. Abort.");
		}

		if (result == 1)
			hi = lo;

		if (lo > hi)
		{
			fprintf(stderr, "timeslot range %s in unexpected format.", ranges[i]);
			die("Abort.");
		}

		if (lo < 1 || hi > 31)
			die("Timeslot outside expected range 1..31. Abort.");

		do
		{
			timeslots[(*n_timeslots)++] = lo++;
		} while (lo <= hi);
	}
}

static void
check_for_overlapping_channels(Channel_t *channels, int n_channels)
{
	int i;
	int j;
	int k;
	int l;

	for (i = 0; i < n_channels; i++)
		for (j = i + 1; j < n_channels; j++)
		{
		if (channels[i].span == channels[j].span)
		{
			for (k = 0; k < channels[i].n_timeslots; k++)
				for (l = 0; l < channels[j].n_timeslots; l++)
					if (channels[i].timeslots[k] == channels[j].timeslots[l])
					{
				fprintf(stderr, "Input %s:%d was specified twice. ",
					channels[i].span, channels[i].timeslots[k]);
				die("Abort.");
					}
		}
		}
}


// See 'usage' for examples of what the channel arguments can look like.
//
// Return the number of arguments consumed
static int
arguments_to_channels(int argc,
char **argv,
Channel_t *channels,
int *n_channels)
{
	char *spans[MAX_SPANS];
	int n_spans = 0;

	int timeslots[MAX_TIMESLOTS];
	int n_timeslots;

	int current_arg = 0;

	int i;

	if (!is_span_name(argv[current_arg]))
		die("Must specify at least one E1/T1 interface, e.g. '1A'. Abort.");

	while (current_arg < argc - 1)
	{
		if (is_span_name(argv[current_arg]))
		{
			if (n_timeslots > 0) // We are starting a new span group
			{
				n_spans = 0;
				n_timeslots = 0;
			}
			spans[n_spans++] = argv[current_arg];
			if (n_spans > MAX_SPANS)
				die("Too many interfaces given");
		}
		else // We're now processing either timeslots or timeslot ranges
		{
			if (n_spans == 0)
				die("Expected E1/T1 interface before further arguments. Abort.");

			argument_to_ts_array(argv[current_arg], timeslots, &n_timeslots);

			for (i = 0; i < n_spans; i++)
			{
				if (*n_channels >= MAX_MTP2_CHANNELS)
					die("Attempted to start too many signalling channels. Abort.");

				channels[*n_channels].span = spans[i];
				channels[*n_channels].n_timeslots = n_timeslots;
				memcpy(channels[*n_channels].timeslots, timeslots,
					sizeof timeslots);
				(*n_channels)++;
			}
		}
		current_arg++;
	}

	check_for_overlapping_channels(channels, *n_channels);

	return current_arg;
}

// Two possible rotation syntaxes.
//
// '-n packets:123' means rotate after 123 packets
// '-n duration:60' means rotate after 60 seconds
//
// The old syntax, e.g. '-n 55' is still supported.
//
// It's possible to specify multiple, separate -n arguments.
void static
parse_rotation(char *arg, int *n_sus_per_file, int *duration_per_file)
{
	int n;

	if (sscanf(arg, "packets:%d", &n) == 1
		|| sscanf(arg, "%d", &n) == 1)
	{
		*n_sus_per_file = n;
		return;
	}

	if (sscanf(arg, "duration:%d", &n) == 1)
	{
		*duration_per_file = n;
		return;
	}

	usage();
}


void static
process_arguments(char **argv,
int argc,
int *monitoring,
int *verbose,
int *n_sus_per_file,
int *duration_per_file,
int *stop_after_interval,
int *drop_fisus,
int *esnf,
char **hostname,
Channel_t channels[],
int *n_channels,
char **base_filename,
enum PCap_format *format)
{
	int current_arg;

	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
		case 'c': *format = PCAP_CLASSIC; break;

		case 'f':
			if (argc < 3) {
				usage();
			}
			if (!strcmp("fisu=no", argv[2])) {
				*drop_fisus = 1;
			}
			else if (!strcmp("esnf=yes", argv[2])) {
				*esnf = 1;
			}
			argc--;
			argv++;
			break;

		case 'm': *monitoring = 1; break;

		case 'n':
			if (argc < 3) {
				usage();
			}
			parse_rotation(argv[2], n_sus_per_file, duration_per_file);
			argc--;
			argv++;
			break;

		case 's': *stop_after_interval = 1; break;

		case 'v': *verbose = 1; break;

		default: usage();
		}
		argc--;
		argv++;
	}

	if (argc < 4) {
		usage();
	}

	*hostname = argv[1];

	argv += 2;
	argc -= 2;

	current_arg = arguments_to_channels(argc, argv, channels, n_channels);
	if (!n_channels){
		die("No timeslots given (or, perhaps, no output filename given).");
	}

	print_channels(channels, *n_channels);

	*base_filename = argv[current_arg];
}

// Entry point
int
main(int argc, char **argv)
{
	GTH_api api;
	int data_socket = -1;
	int result;
	int monitoring = 0;
	int verbose = 0;
	Channel_t channels[MAX_MTP2_CHANNELS];
	int i;
	int n_channels = 0;
	int n_sus_per_file = 0;
	int duration_per_file = 0;
	int stop_after_interval = 0;
	int drop_fisus = 0;
	int esnf = 0;
	int listen_port = 0;
	int listen_socket = -1;
	enum PCap_format format = PCAP_NG;
	char *hostname;
	char *base_filename;

	// Check a couple of assumptions about type size.
	assert(sizeof(u32) == 4);
	assert(sizeof(u16) == 2);

	win32_specific_startup();

	process_arguments(argv, argc,
		&monitoring, &verbose, &n_sus_per_file, &duration_per_file, &stop_after_interval,
		&drop_fisus, &esnf, &hostname, channels, &n_channels,
		&base_filename, &format);
	result = gth_connect(&api, hostname, verbose);
	if (result != 0) {
		die("Unable to connect to the GTH. Giving up.");
	}

	read_hw_description(&api, hostname);
	enable_l1(&api, channels, n_channels, monitoring);

	listen_socket = gth_make_listen_socket(&listen_port);
	for (i = 0; i < n_channels; i++){
		monitor_mtp2(&api, channels + i,
			i, drop_fisus, esnf, listen_port, listen_socket);
		if (i == 0) {
			data_socket = gth_wait_for_accept(listen_socket);
		}
	}

	fprintf(stderr, "capturing packets, press ^C to abort\n");
	convert_to_pcap(&api, data_socket, base_filename,
		n_sus_per_file, duration_per_file, stop_after_interval,
		channels, n_channels, format);

	return 0; // not reached
}

// eof
