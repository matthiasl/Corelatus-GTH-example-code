//----------------------------------------------------------------------
// An example program. Shows how to capture SS7 data off the wire (an E1/T1)
// with a GTH and save it to a file (or stdout) in either PCap-NG or
// classic PCap format for further analysis with e.g. wireshark or tshark.
//
// References:
//
//  PCap-NG doc: https://github.com/pcapng/pcapng/
// classic PCap: http://wiki.wireshark.org/Development/LibpcapFileFormat
//      GTH API: https://www.corelatus.com/gth/api/
//
// Limitations:
//
//  Doesn't attempt any API error handling.
//  Doesn't check or keep track of L1 status
//  Doesn't log L1 or L2 errors and state changes
//
// Author: Matt Lang (matthias@corelatus.se)
// Contributions from: Arash Dalir
//
// Copyright (c) 2009--2016, Corelatus AB
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
#include <stdint.h>
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
#include "pcap_structs.h"

#define MAX_FILENAME 100
#define MAX_TIMESTAMP 40

// Longest possible signal unit is AAL5; limited to 64k by the standard,
// but GTH limits it to 4k (which is reasonable for signalling)
#define MAX_SIGNAL_UNIT 4200

// There are 31 useable timeslots in an E1
#define MAX_TIMESLOTS 31

// E1/T1 Monitor 2.x has 16 E1/T1 inputs
// E1/T1 Monitor 3.0 has 64
// SDH Monitor 3.0 has 128 E1/T1 inputs
// SDH Monitor 3.0 has 2 VC-4 (for ATM)
#define MAX_SOURCES 128

// How many channels can we run at the same time?
//
// The maximum differs depending on hardware and protocol, if exceeded
// the port-2089 API will return a 'refused' error with reason 'capacity.
#define MAX_CHANNELS 400

#pragma pack(1)
struct GTH_mtp2_lapd_raw {
  char payload[MAX_SIGNAL_UNIT];
};

struct GTH_aal0 {
  u32 gfc_vpi_vci;
  char payload[MAX_SIGNAL_UNIT];
};

struct GTH_aal2 {
  u32 gfc_vpi_vci;
  // The first 3 bytes of the payload are the
  // CID, LI, UUI and a HEC.
  char payload[MAX_SIGNAL_UNIT];
};

struct GTH_aal5 {
  u32 gfc_vpi_vci;
  u8 cpcs_uu;
  u8 cpi;
  u16 cpcs_length;
  u32 crc;
  char payload[MAX_SIGNAL_UNIT];
};

struct GTH_su {
  u16 tag;
  u16 flags;
  u16 timestamp_hi;
  u32 timestamp_lo;

  union {
    struct GTH_mtp2_lapd_raw ml;
    struct GTH_aal2          a2;
    struct GTH_aal5          a5;
  };
};

enum PCap_format { PCAP_CLASSIC = 0, PCAP_NG };

enum Filename_format { FF_DEFAULT = 0, FF_UTC, FF_LOCALTIME };

enum Protocol {
  MTP2,
  LAPD,
  AAL0,
  AAL2,
  AAL5,
  RAW
};

typedef struct {
  char *source;    // Either an E1 span or an SDH source
  int timeslots[MAX_TIMESLOTS];
  int n_timeslots;
} Channel_t;

// Command-line options provided by the user
struct Options {
  int monitoring;
  int skip_l1_setup;
  int verbose;
  int n_sus_per_file;
  int duration_per_file;
  int drop_fisus;
  int esnf;
  int vpi;
  int vci;
  int bandwidth;
  char *hostname;
  Channel_t channels[MAX_CHANNELS];
  int n_channels;
  enum PCap_format format;
  enum Protocol protocol;
  enum Link_type link_type;

  int write_to_stdout;
  int write_to_winpipe;
  int capture_autostop;
  enum Filename_format filename_format;
  char *base_filename;

};

static struct Options options;

static GTH_event_handler *default_event_handler;

//----------------------------------------------------------------------
static void
usage(void) {
  fprintf(stderr,
	  "save_to_pcap git_head: %s build_hostname: %s\n\n"

	  "save_to_pcap <options> <GTH-IP> <channels> [<channels>...] <filename>"
	  "\n\nSave decoded signal units to a file in libpcap format, suitable for"
	  "\nexamining with wireshark, tshark or other network analyser software.\n"
	  "\n<options>: [-a vpi:vci] [-c] [-f <option>] [-l] [-m] [-n <rotation>]"
	  "\n           [-p mtp2|lapd|aal0|aal2|aal5|raw] [-s] [-v]\n"
	  "\n-a <vpi:vci>: the ATM VPI and VCI (used together with -p aal5)"
          "\n-b 56|48 bandwidth, in kbit/s (used for ANSI/NTT subrate MTP-2)"
	  "\n-c: save in the classic Pcap format (default is the newer Pcap-NG)"
	  "\n-f fisu=no: remove all MTP-2 FISUs"
	  "\n-f esnf=yes: use MTP-2 extended sequence numbers"
	  "\n-l skip layer 1 setup; assume it is already done"
	  "\n-m: tells the GTH that you are using a -20dB monitor point"
	  "\n-n <packets:c>: rotate the output file after <c> packets"
	  "\n-n <duration:s>: rotate the output file after <s> seconds"
          "\n-p mtp2|lapd|aal0|aal2|aal5|raw: select the signalling protocol"
          "\n-s: stop capturing (terminate) instead of rotating files"
          "\n-t utc=yes: append the UTC time to the filename"
          "\n-t utc=no: append the local time to the filename"
	  "\n-v: print API commands and responses (verbose)"
	  "\n"
	  "\n<GTH-IP> is the GTH's IP address or hostname"
	  "\n<channels> for E1/T1 interfaces is a list of spans and timeslots:"
	  "\n  <span> [<span>...] <timeslot> [<timeslot>...]"
	  "\n<span> is the name of a span, e.g. '1A'"
	  "\n<timeslot> is a timeslot number, from 1 to 31"
	  "\n  e.g. 1A 2A 1 2 3 monitors timeslots 1, 2 and 3 on span 1A and 2A."
	  "\n  e.g. 1A 1 2A 2 3A 3 4 monitors timeslot 1 on 1A, 2 on 2A and 3 and 4 on 3A."
	  "\n<channels> can also be a range of timeslots (for Nx64kbit/s signalling):"
	  "\n  <span> <timeslot>-<timeslot>[,<timeslot>-<timeslot>]"
	  "\n  e.g. 1A 1-31 monitors one 1980kbit/s channel on timeslots 1-31"
	  "\n  e.g. 1A 1-15,17-31 monitors one 1920kbit/s channel"
	  "\n  e.g. 1A 1-4 1B 1-4 monitors two 256kbit/s channels"
	  "\n"
	  "\n<channels> for SDH VC-4 or VC-3 is the name of a high-order-path:"
	  "\n  e.g. sdh1:hop1_1 monitors the first VC-4 or VC-3 on sdh1"
	  "\n"
	  "\n<filename> can be -, which means standard output.\n\n",
	  git_head, build_hostname);

  fprintf(stderr,
	  "Examples (on hardware with electrical E1/T1 ports):\n"
	  "./save_to_pcap 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
          "./save_to_pcap -p lapd 172.16.1.10 1A 2A 16 lapd_capture.pcapng\n"
          "./save_to_pcap -p raw  172.16.1.10 1A 2A 16 raw_capture.pcapng\n"
	  "./save_to_pcap 172.16.1.10 1A 2A 3A 4A 1 2 3 4 isup_capture.pcapng\n"
	  "./save_to_pcap 172.16.1.10 1A 2A 16 1B 5 6 7 8 capture.pcapng\n"
	  "./save_to_pcap 172.16.1.10 1A 1-31 mtp2_annex_a.pcapng\n"
	  "./save_to_pcap -f esnf=yes 172.16.1.10 1A 1-31 mtp2_annex_a_esnf.pcapng\n"
	  "./save_to_pcap -m 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
	  "./save_to_pcap -m -n packets:1000 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
	  "./save_to_pcap -m -n duration:60 172.16.1.10 1A 2A 16 isup_capture.pcapng\n"
	  "./save_to_pcap -c 172.16.1.10 1A 2A 16 - | tshark -V -i - \n"
	  "./save_to_pcap -c 172.16.1.10 1A 2A 16 - | wireshark -k -i - \n"
	  "./save_to_pcap -c 172.16.1.10 1A 2A 16 \\\\.\\pipe\\isup_capture.1\n");

  fprintf(stderr,
	  "\nExample (E1 carried in a VC-12, usually optical):\n"
	  "./save_to_pcap 172.16.1.10 pcm55 16 isup_capture.pcapng\n");

    fprintf(stderr,
	  "\nExample (ATM AAL5 in a VC-4 on SDH):\n"
	  "./save_to_pcap -p aal5 -a 0:5 172.16.1.10 sdh1:hop1_1 aal5_capture.pcapng\n");

    fprintf(stderr,
            "\nExample (ATM AAL2 on an E1):\n"
            "./save_to_pcap -p aal2 -a 0:7 172.16.1.10 1A 1-15,17-31 aal2_capture.pcapng\n");

    fprintf(stderr,
            "\nExample (ATM AAL0 on an E1):\n"
            "./save_to_pcap -p aal0 172.16.1.10 1A 1-15,17-31 aal0_capture.pcapng\n\n");

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
			size_t length,
			size_t items,
			HANDLE_OR_FILEPTR file)
{
  int result;
  DWORD written;

  result = WriteFile(file, buffer, length * items, &written, 0);
  if (written != length || !result ) {
    return 0;
  }
  return 1;
}

static HANDLE_OR_FILEPTR
stdout_handle_or_file(void)
{
  return GetStdHandle(STD_OUTPUT_HANDLE);
}

#define fclose CloseHandle

#else
static HANDLE_OR_FILEPTR
stdout_handle_or_file(void)
{
  return stdout;
}

#endif

#ifdef WIN32
static HANDLE_OR_FILEPTR
open_windows_pipe(const char *filename)
{
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
}
#else
static HANDLE_OR_FILEPTR
open_windows_pipe(const char *filename)
{
  die("Cannot open a windows named pipe on a non-windows OS. Giving up.");
  die(filename);  // not reached
  return 0;
}
#endif

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
  fprintf(stderr, "saving to file %s\n", filename);
}

//----------------------------------------------------------------------

static int
is_sdh_name(const char *arg)
{
  return (strstr(arg, "sdh") != 0);   // Optical hardware uses e.g. 'sdh1:hop1_1'
}

static int
is_span_name(const char *arg)
{
  if (strstr(arg, "pcm")) return 1;  // Optical hardware uses e.g. 'pcm55'
  if (strstr(arg, "A")) return 1;
  if (strstr(arg, "B")) return 1;
  if (strstr(arg, "C")) return 1;
  if (strstr(arg, "D")) return 1;
  return 0;
}

// Optical E1/T1: just enable; no special options required.
static void
enable_optical_l1(GTH_api *api,
		  const char* source,
		  const int monitoring)
{
  int result;
  char source_name[20];
  GTH_const_attribute attributes[] = { {"payload", "ATM"} };
  int n_attributes = 0;

  strncpy_s(source_name, 20, source, 19);

  // If we're decoding ATM from a VC-4 or a VC-3, the resource will be called
  // something like sdh1:hop1_1. We want to truncate at the colon. We also
  // need to set the payload to 'ATM'.
  if (is_sdh_name(source)) {
    source_name[4] = 0;
    n_attributes = 1;
  }

  if (monitoring)
    fprintf(stderr, "Warning: ignoring -m switch on optical hardware.\n");

  result = gth_enable(api, source_name, attributes, n_attributes);

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

  int n_attributes = (monitoring)?3:2;
  GTH_const_attribute attributes[] = { {"status", "enabled"},
                                       {"tx_enabled", "false"},
                                       {"monitoring", "true"}
  };

  assert(sizeof(span_name) > (strlen(span) + strlen("pcm")));
  strncpy_s(span_name, sizeof span_name, "pcm", sizeof span_name - 1);
  strncat(span_name, span, sizeof span_name - strlen(span_name) - 1);

  // Use <set> here: <enable> isn't supported until gth2_system_37a.
  result = gth_set(api, span_name, attributes, n_attributes);

  if (result != 0)
    die("Setting up L1 failed. (-v switch gives more information)");
}

// Build an array with a sorted list of unique sources
// tactic: insertion sort. Uses a linear search. Fast enough in practice,
//         sorting 400 channels with 64 E1s takes less than 1 ms.
static void
build_sources_list(const Channel_t channels[], const int n_channels,
                   char *sources[], int *n_sources)
{
  int channel;
  int source;
  int i;

  *n_sources = 0;

  for (channel = 0; channel < n_channels; channel++) {
    int compare = 1;

    // Find insertion point
    for (source = 0; source < *n_sources; source++) {
      compare = strcmp(sources[source], channels[channel].source);
      if (compare >= 0) break;
    }

    if (compare == 0) continue;   // source already present

    // Make space for insertion
    (*n_sources)++;
    for (i = *n_sources; i > source; i--)
      sources[i] = sources[i-1];

    // Insert
    sources[i] = channels[channel].source;
  }
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
  char *sources[MAX_SOURCES];
  int n_sources;

  build_sources_list(channels, n_channels, sources, &n_sources);

  result = gth_query_resource_attribute(api, "board", "architecture",
					architecture, 10);
  if (result != 0)
    die("Unable to query hardware architecture. Giving up.");

  architecture[3] = 0;
  if (strcmp(architecture, "gth") == 0) {
    for (i = 0; i < n_sources; i++) {
      enable_electrical_l1(api, sources[i], monitoring);
    }
  }
  else {
    for (i = 0; i < n_sources; i++) {
      enable_optical_l1(api, sources[i], monitoring);
    }
  }
}

#define MAX_MTP2_ATTRS 2

// Start up MTP-2 monitoring
static void
monitor_mtp2(GTH_api *api,
	     const Channel_t *channel,
	     int tag,
	     int listen_port
	     )
{
  int result;
  char job_id[MAX_JOB_ID];
  GTH_attribute attrs[MAX_MTP2_ATTRS];
  int n_attrs = 0;

  if (options.drop_fisus) {
    attrs[n_attrs].key = "fisu";
    attrs[n_attrs].value = "no";
    n_attrs++;
  }

  if (options.esnf) {
    attrs[n_attrs].key = "esnf";
    attrs[n_attrs].value = "yes";
    n_attrs++;
  }

  result = gth_new_mtp2_monitor_opt(api, tag,
				    channel->source,
				    channel->timeslots,
				    channel->n_timeslots,
                                    options.bandwidth,
				    job_id, api->my_ip, listen_port,
				    attrs, n_attrs);
  if (result != 0)
    die("Setting up MTP2 monitoring failed. (-v gives more information)");

  return;
}

// Start up LAPD monitoring
static void
monitor_lapd(GTH_api *api,
	     const Channel_t *channel,
	     int tag,
	     int listen_port
	     )
{
  int result;
  char job_id[MAX_JOB_ID];

  result = gth_new_lapd_monitor(api, tag,
                                channel->source,
                                channel->timeslots[0],
                                job_id, api->my_ip, listen_port);
  if (result != 0)
    die("Setting up LAPD monitoring failed. (-v gives more information)");

  return;
}

// Start up AAL2 monitoring
static void
monitor_aal0(GTH_api *api,
	     const Channel_t *channel,
	     int tag,
	     int listen_port
	     )
{
  int result;
  char job_id[MAX_JOB_ID];

  if (strstr(channel->source, "sdh")) {
    die("AAL0 monitoring is not currently supported in a VC-4 or VC-3");
  }

  result = gth_new_atm_aal0_monitor(api, tag,
                                    channel->source,
                                    channel->timeslots,
                                    channel->n_timeslots,
                                    job_id, api->my_ip, listen_port);

  if (result != 0)
    die("Setting up AAL0 monitoring failed. (-v gives more information)");

  return;
}


// Start up AAL2 monitoring
static void
monitor_aal2(GTH_api *api,
	     const Channel_t *channel,
	     int tag,
	     int listen_port
	     )
{
  int result;
  char job_id[MAX_JOB_ID];

  if (strstr(channel->source, "sdh")) {
    result = gth_new_sdh_atm_aal2_monitor(api, tag,
					  channel->source,
					  options.vpi,
					  options.vci,
					  job_id, api->my_ip, listen_port);
  }
  else {
    result = gth_new_atm_aal2_monitor(api, tag,
				      channel->source,
				      channel->timeslots,
				      channel->n_timeslots,
				      options.vpi,
				      options.vci,
				      job_id, api->my_ip, listen_port);
  }

  if (result != 0)
    die("Setting up AAL2 monitoring failed. (-v gives more information)");

  return;
}


// Start up AAL5 monitoring
static void
monitor_aal5(GTH_api *api,
	     const Channel_t *channel,
	     int tag,
	     int listen_port
	     )
{
  int result;
  char job_id[MAX_JOB_ID];

  if (strstr(channel->source, "sdh")) {
    result = gth_new_sdh_atm_aal5_monitor(api, tag,
					  channel->source,
					  options.vpi,
					  options.vci,
					  job_id, api->my_ip, listen_port);
  }
  else {
    result = gth_new_atm_aal5_monitor(api, tag,
				      channel->source,
				      channel->timeslots,
				      channel->n_timeslots,
				      options.vpi,
				      options.vci,
				      job_id, api->my_ip, listen_port);
  }

  if (result != 0)
    die("Setting up AAL5 monitoring failed. (-v gives more information)");

  return;
}

// Start up RAW monitoring
static void
monitor_raw(GTH_api *api,
            const Channel_t *channel,
            int tag,
            int listen_port
            )
{
  int result;
  char job_id[MAX_JOB_ID];

  result = gth_new_raw_monitor(api, tag,
                               channel->source,
                               channel->timeslots[0],
                               job_id, api->my_ip, listen_port);
  if (result != 0)
    die("Setting up raw monitoring failed. (-v gives more information)");

  return;
}

// Read exactly the requested number of bytes from the given descriptor
static void
read_exact(int fd, char* buf, size_t count)
{
  ssize_t this_time;

  while (count > 0) {
    this_time = recv(fd, buf, count, 0);
    if (this_time <= 0)
      die("Signal unit socket from GTH unexpectedly closed\n");

    count -= this_time;
    buf += this_time;
  }
}

POSSIBLY_EXTERN inline void
checked_fwrite(void *b, int n, HANDLE_OR_FILEPTR f)
{
  int result = fwrite(b, n, 1, f);

  if (result != 1) {
    die("fwrite failed. (Is the output file writeable?)");
  }
}


static void
write_pcap_classic_header(HANDLE_OR_FILEPTR file, enum Link_type link_type)
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
  header.network = link_type;

  checked_fwrite((void*)&header, sizeof header, file);

  return;
}

// Must be a multiple of 4 because of padding rules in PCap-ng
#define MAX_HW_DESCRIPTION 400
static char hw_description[MAX_HW_DESCRIPTION];

static void
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

inline static int
round_up_32_bit(int x)
{
  return (x + 3) & (~3);
}

static void
write_pcap_ng_shb(HANDLE_OR_FILEPTR file)
{
  PCap_NG_shb shb;
  u32 btl;
  PCap_NG_option userappl = {4, 0};        // 4 = user application name
  PCap_NG_option end_of_options = {0, 0};
  userappl.length = strlen(hw_description);

  btl = sizeof(PCap_NG_shb)
    + sizeof(userappl) + round_up_32_bit(userappl.length)
    + sizeof(end_of_options)
    + sizeof(shb.block_total_length);

  shb.type                  = 0x0A0D0D0A;
  shb.block_total_length    = btl;
  shb.byte_order_magic      = 0x1A2B3C4D;
  shb.major_version         = 1;
  shb.minor_version         = 0;
  shb.section_length        = 0xffffffffFFFFFFFFULL; // "unknown"

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
write_pcap_idbs(HANDLE_OR_FILEPTR file, const Channel_t *c, int n,
                int link_type)
{
  int x;
  PCap_NG_option end_of_options = {0, 0};

  for (x = 0; x < n; x++) {
    PCap_NG_idb idb;
    PCap_NG_option if_name;
    char idb_if_name[MAX_IF_NAME];
    PCap_NG_option if_tsresol;
    char tsresol[4] = {3, 0, 0, 0}; // 10^-3 resolution, three padding bytes

    u32 block_total_length;

    if_name.code = 2;    // if_name
    if (strstr(c[x].source, "sdh")) {
      if_name.length = snprintf(idb_if_name, MAX_IF_NAME, "%s", c[x].source);
    }
    else {
      if_name.length = snprintf(idb_if_name, MAX_IF_NAME, "%s:%d",
			      c[x].source, c[x].timeslots[0]);
    }
    if (if_name.length >= MAX_IF_NAME)
      die("interface name is too long");

    block_total_length = sizeof(idb)
      + sizeof(PCap_NG_option) + round_up_32_bit(if_name.length)
      + sizeof(PCap_NG_option) + sizeof(tsresol)
      + sizeof(PCap_NG_option)
      + sizeof(block_total_length);

    idb.type               = PCAPNG_BLOCK_TYPE_IDB;
    idb.block_total_length = block_total_length;
    idb.link_type          = link_type;
    idb.reserved           = 0;
    idb.snaplen            = MAX_SIGNAL_UNIT;

    if_tsresol.code = 9; // tsresol
    if_tsresol.length = 1;

    checked_fwrite((void*)&idb, sizeof(idb), file);

    checked_fwrite((void*)&if_name, sizeof (if_name), file);
    checked_fwrite((void*)&idb_if_name, round_up_32_bit(if_name.length), file);

    checked_fwrite((void*)&if_tsresol, sizeof(if_tsresol), file);
    checked_fwrite((void*)&tsresol, sizeof(tsresol), file);

    checked_fwrite((void*)&end_of_options, sizeof(end_of_options), file);

    checked_fwrite((void*)&block_total_length, sizeof block_total_length, file);
  }
}

static void
write_pcap_global_header(HANDLE_OR_FILEPTR file,
			 const struct Options *user_options)
{
  switch (user_options->format) {
  case PCAP_CLASSIC:
    write_pcap_classic_header(file, user_options->link_type);
    break;

  case PCAP_NG:
    write_pcap_ng_shb(file);
    write_pcap_idbs(file,
                    user_options->channels,
                    user_options->n_channels,
                    user_options->link_type);
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
  uint64_t ts_sec;
  uint64_t ts_us;
  int result;

  assert(sizeof ts_sec == 8);

  ts_us = timestamp_hi;
  ts_us <<= 32;
  ts_us += timestamp_lo;

  ts_sec = ts_us / 1000;
  ts_us = (ts_us % 1000) * 1000;

  pcap_header.ts_sec = (u32)ts_sec;
  pcap_header.ts_us =  (u32)ts_us;
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

  epb.type = PCAPNG_BLOCK_TYPE_EPB;
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
  return (a > b)?a:b;
}



// Block (or loop) until a packet arrives.
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
      struct timeval tv = {1, 0};
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
          gth_process_event(api);
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
  timer = CreateWaitableTimer(NULL, TRUE, NULL);
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
  struct itimerspec new_timer = { {0, 0}, {seconds, 0} };

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
keep_capturing(int su_count, struct Options opts)
{
  if  (opts.n_sus_per_file > 0)
    {
      return (su_count < opts.n_sus_per_file);
    }

  if (opts.duration_per_file > 0)
    {
      return (read_and_restart_timer(opts.duration_per_file) != 0);
    }

  return 1;
}

static void
write_mtp2(HANDLE_OR_FILEPTR file, struct GTH_su *signal_unit, int length)
{
  length -= (signal_unit->ml.payload - (char*)&(signal_unit->tag));

  write_packet(file,
	       ntohs(signal_unit->timestamp_hi),
	       ntohl(signal_unit->timestamp_lo),
	       ntohs(signal_unit->tag),
	       signal_unit->ml.payload,
	       length,
	       options.format);
  flush_file(file);
}

// For wireshark, LAPD is almost the same as MTP-2. The only difference
// is that we must discard the two-octet FCS (CRC).
static void
write_lapd(HANDLE_OR_FILEPTR file, struct GTH_su *signal_unit, int length)
{
  write_mtp2(file, signal_unit, length - 2);
}

// Wireshark doesn't have anything for AAL0. So just dump the raw data.
static void
write_aal0(HANDLE_OR_FILEPTR file, struct GTH_su *signal_unit, int length)
{
  write_mtp2(file, signal_unit, length);
}

// Raw data. Just dump it.
static void
write_raw(HANDLE_OR_FILEPTR file, struct GTH_su *signal_unit, int length)
{
  write_mtp2(file, signal_unit, length);
}

static void
write_aal2(HANDLE_OR_FILEPTR file, struct GTH_su *signal_unit, int length)
{
  u8 payload[MAX_SIGNAL_UNIT];
  struct NG40_aal2_header ng40;
  u32 vpi = (signal_unit->a2.gfc_vpi_vci >> 20) & 0xff;
  u32 vci = (signal_unit->a2.gfc_vpi_vci >> 4) & 0xffff;

  length -= (  (char*)&(signal_unit->a2.payload)
	     - (char*)&(signal_unit->tag));

  ng40.type = 2;      // AAL2
  ng40.length = sizeof(ng40) + length - 3;
  ng40.protocol = 1;  // 1=ALCAP 2=NBAP
  ng40.id = 0;
  ng40.flags = 0;     // 0x1: message is ciphered
  ng40.direction = 0;
  ng40.vpi = vpi;
  ng40.vci = vci;
  ng40.cid = signal_unit->a2.payload[0];

  memcpy(payload, &ng40, sizeof(ng40));

  memcpy(payload + sizeof(struct NG40_aal2_header),
         signal_unit->a2.payload + 3, length);

  write_packet(file,
	       ntohs(signal_unit->timestamp_hi),
	       ntohl(signal_unit->timestamp_lo),
	       ntohs(signal_unit->tag),
	       payload,
	       length + sizeof(struct NG40_aal2_header) - 3,
	       options.format);
  flush_file(file);
}


static void
write_aal5(HANDLE_OR_FILEPTR file, struct GTH_su *signal_unit, int length)
{
  u8 payload[MAX_SIGNAL_UNIT];

  length -= (  (char*)&(signal_unit->a5.payload)
	     - (char*)&(signal_unit->tag));

  // We copy the whole packet to insert the SUNATM header bytes.
  // This is cleaner but slightly slower than just trashing half the CRC
  memcpy(payload + 4, signal_unit->a5.payload, length);
  payload[0] = 6; // Traffic is SAAL/Q.2931
  payload[1] = options.vpi;
  payload[2] = options.vci >> 8;
  payload[3] = options.vci & 0xff;

  write_packet(file,
	       ntohs(signal_unit->timestamp_hi),
	       ntohl(signal_unit->timestamp_lo),
	       ntohs(signal_unit->tag),
	       payload,
	       length + 4,
	       options.format);
  flush_file(file);
}

static void
make_file_timestamp(const struct Options *opts, char *string)
{
  time_t rawtime;
  struct tm * timeinfo;

  time(&rawtime);

  switch (opts->filename_format) {
  case FF_DEFAULT:
    *string = 0;
    return;
    break;

  case FF_UTC:
    timeinfo = gmtime(&rawtime);  // REVISIT: not threadsafe
    break;

  case FF_LOCALTIME: /* fall through */
  default:
    timeinfo = localtime(&rawtime);  // REVISIT: not threadsafe
    break;

  }

  snprintf(string, MAX_TIMESTAMP, "_%04d%02d%02d%02d%02d%02d",
           timeinfo->tm_year + 1900,
           timeinfo->tm_mon + 1,
           timeinfo->tm_mday,
           timeinfo->tm_hour,
           timeinfo->tm_min,
           timeinfo->tm_sec);
}

static HANDLE_OR_FILEPTR
open_packet_file(const struct Options *opts, int *file_number)
{
    char filename[MAX_FILENAME];
    char timestamp[MAX_TIMESTAMP];
    HANDLE_OR_FILEPTR file;

    if (opts->write_to_stdout)
      {
	file = stdout_handle_or_file();
	fprintf(stderr, "saving capture to stdout\n");
      }
    else if (opts->write_to_winpipe)
      {
	fprintf(stderr, "saving capture to a windows named pipe\n");
	file = open_windows_pipe(opts->base_filename);
      }
    else
      {
        if (opts->capture_autostop)
          {
            open_file_for_writing(&file, opts->base_filename);
          }
        else
          {
            make_file_timestamp(opts, timestamp);
            snprintf(filename, MAX_FILENAME, "%s_%05d%s",
                     opts->base_filename,
                     *file_number,
                     timestamp);
            open_file_for_writing(&file, filename);
          }

        (*file_number)++;
      }

    return file;
}


// Loop, converting the incoming GTH data to libpcap format
static void
convert_to_pcap(GTH_api *api,
		int data_socket,
                const struct Options user_options)
{
  u16 length;
  struct GTH_su signal_unit;
  int su_count;
  int file_number = 1;
  HANDLE_OR_FILEPTR file;

  init_timer(options.duration_per_file);

  do {
    file = open_packet_file(&user_options, &file_number);
    write_pcap_global_header(file, &user_options);
    su_count = 0;

    do
      {
	if (wait_for_packet(api, data_socket) != 0)
	  {
	    read_exact(data_socket, (void*)&length, sizeof length);
	    length = ntohs(length);
	    assert(length <= sizeof signal_unit);
	    read_exact(data_socket, (void*)&signal_unit, length);

	    switch (user_options.protocol) {
            case AAL0: write_aal0(file, &signal_unit, length); break;
            case AAL2: write_aal2(file, &signal_unit, length); break;
	    case AAL5: write_aal5(file, &signal_unit, length); break;
	    case MTP2: write_mtp2(file, &signal_unit, length); break;
	    case LAPD: write_lapd(file, &signal_unit, length); break;
            case RAW:  write_raw( file, &signal_unit, length); break;
	    }
	    su_count++;
	  }
      }
    while ( keep_capturing(su_count, user_options) );
    fclose(file);
  }
  while ( !user_options.capture_autostop );
}

static void
print_channels(Channel_t *channels, int n)
{
  int x;
  int y;

  for (x = 0; x < n; x++) {
    fprintf(stderr, "monitoring %s:", channels->source);
    for (y = 0; y < channels->n_timeslots-1; y++) {
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

  while ( (pos = strchr(s, ',')) )
    {
      strncpy_s(ranges[range++], MAX_RANGE, s, pos-s);
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
	}
      while (lo <= hi);
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
    for (j = i+1; j < n_channels; j++)
      {
	if (channels[i].source == channels[j].source)
	  {
	    for (k = 0; k < channels[i].n_timeslots; k++)
	      for (l = 0; l < channels[j].n_timeslots; l++)
		if (channels[i].timeslots[k] == channels[j].timeslots[l])
		  {
		    fprintf(stderr, "Input %s:%d was specified twice. ",
			    channels[i].source, channels[i].timeslots[k]);
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
  char *sources[MAX_SOURCES];      // either an E1/T1 span or an SDH VC-4/VC-3
  int n_sources = 0;

  int timeslots[MAX_TIMESLOTS];
  int n_timeslots = 0;

  int current_arg = 0;

  int i;

  if (is_sdh_name(argv[current_arg]))
    {
      channels[*n_channels].source = argv[current_arg];
      (*n_channels)++;
      return 1;
    }

  if (!is_span_name(argv[current_arg]))
    die("Must specify at least one E1/T1 or SDH interface, e.g. '1A'. Abort.");

  while (current_arg < argc - 1)
    {

      if (is_span_name(argv[current_arg]))
	{
	  if (n_timeslots > 0) 	  // starting a new span group.
	    {
	      n_sources = 0;
	      n_timeslots = 0;
	    }
	  sources[n_sources++] = argv[current_arg];
	  if (n_sources > MAX_SOURCES)
	    die("Too many interfaces given");
	}
      else // We're now processing either timeslots or timeslot ranges
	{
	  if (n_sources == 0)
	    die("Expected E1/T1 or SDH interface. Abort.");

	  argument_to_ts_array(argv[current_arg], timeslots, &n_timeslots);

	  for (i = 0; i < n_sources; i++)
	    {
	      if (*n_channels >= MAX_CHANNELS)
		die("Attempted to start too many signalling channels. Abort.");

	      channels[*n_channels].source = sources[i];
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
static void
parse_rotation(char *arg, struct Options *opts)
{
  int n;

  if (sscanf(arg, "packets:%d", &n) == 1
      || sscanf(arg, "%d", &n) == 1)
    {
      opts->n_sus_per_file = n;
      return;
    }

  if (sscanf(arg, "duration:%d", &n) == 1)
    {
      opts->duration_per_file = n;
      return;
    }

  usage();
}

static void
process_arguments(char **argv,
		  int argc,
                  struct Options *opts)
{
  int current_arg;
  int result;

  memset(opts, 0, sizeof(struct Options));
  opts->format = PCAP_NG;
  opts->protocol = MTP2;
  opts->link_type = LINK_TYPE_MTP2;
  opts->bandwidth = 64;

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'a':
      if (argc < 3) {
	usage();
      }
      result = sscanf(argv[2], "%d:%d", &(opts->vpi), &(opts->vci));
      if (result != 2) {
	usage();
      }
      argc--;
      argv++;
      break;

    case 'b':
      if (argc < 3) {
	usage();
      }
      result = sscanf(argv[2], "%d", &(opts->bandwidth));
      if (result != 1) {
	usage();
      }
      argc--;
      argv++;
      break;

    case 'c': opts->format = PCAP_CLASSIC; break;

    case 'f':
      if (argc < 3) {
	usage();
      }
      if (!strcmp("fisu=no", argv[2])) {
	opts->drop_fisus = 1;
      } else if (!strcmp("esnf=yes", argv[2])) {
	opts->esnf = 1;
      }
      argc--;
      argv++;
      break;

    case 'l': opts->skip_l1_setup = 1; break;

    case 'm': opts->monitoring = 1; break;

    case 'n':
      if (argc < 3) {
	usage();
      }
      parse_rotation(argv[2], opts);
      argc--;
      argv++;
      break;

    case 's': opts->capture_autostop = 1; break;

    case 't':
      if (argc < 3) {
        usage();
      }
      if (!strcmp("utc=yes", argv[2])) {
        opts->filename_format = FF_UTC;
      }
      else if (!strcmp("utc=no", argv[2])) {
        opts->filename_format = FF_LOCALTIME;
      }
      else {
        usage();
      }

      argc--;
      argv++;
      break;

    case 'p':
      if (argc < 3) usage();
      if      (!strcmp("mtp2", argv[2])) {
	opts->protocol = MTP2;
	opts->link_type = LINK_TYPE_MTP2;
      }
      else if (!strcmp("lapd", argv[2])) {
	opts->protocol = LAPD;
	opts->link_type = LINK_TYPE_LAPD;
      }
      else if (!strcmp("aal0", argv[2])) {
	opts->protocol = AAL0;
	opts->link_type = 0;
      }
      else if (!strcmp("aal2", argv[2])) {
	opts->protocol = AAL2;
	opts->link_type = LINK_TYPE_NG40;
      }
      else if (!strcmp("aal5", argv[2])) {
	opts->protocol = AAL5;
	opts->link_type = LINK_TYPE_SUNATM;
      }
      else if (!strcmp("raw", argv[2])) {
	opts->protocol = RAW;
	opts->link_type = LINK_TYPE_UNKNOWN;
      }

      else usage();
      argc--;
      argv++;
      break;

    case 'v': opts->verbose = 1; break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc < 4) {
    usage();
  }

  opts->hostname = argv[1];

  argv += 2;
  argc -= 2;

  current_arg = arguments_to_channels(argc, argv,
                                      opts->channels,
                                      &(opts->n_channels));
  if (!opts->n_channels){
    die("No timeslots given (or, perhaps, no output filename given).");
  }

  print_channels(opts->channels, opts->n_channels);

  opts->base_filename = argv[current_arg];
  if (strlen(opts->base_filename) > (MAX_FILENAME - MAX_TIMESTAMP - 10)) {
    die("output filename is ridiculously long");
  }
  opts->write_to_stdout = (strcmp(opts->base_filename, "-") == 0);
  opts->write_to_winpipe = is_filename_a_pipe(opts->base_filename);
}

typedef void(*Start_function)
(GTH_api *, const Channel_t*, int, int);

static Start_function
lookup_start_function(enum Protocol protocol)
{
  switch (protocol) {
  case AAL0: return &monitor_aal0; break;
  case AAL2: return &monitor_aal2; break;
  case AAL5: return &monitor_aal5; break;
  case LAPD: return &monitor_lapd; break;
  case MTP2: return &monitor_mtp2; break;
  case RAW:  return &monitor_raw;  break;
  }

  die("can't find start function for link type");

  return 0;  // not reached
}

static void event_handler(void *data, GTH_resp *resp)
{
  GTH_resp *child;
  GTH_api *api = data;

  assert(api);
  assert(resp);
  assert(resp->type == GTH_RESP_EVENT);
  assert(resp->n_children == 1);

  child = resp->children + 0;

  switch (child->type) {

  case GTH_RESP_SDH_MESSAGE: {
    // suppress SDH messages; use the -v switch to see them
    break;
  }

  case GTH_RESP_ATM_MESSAGE:     /* fall through */
  case GTH_RESP_F_RELAY_MESSAGE: /* fall through */
  case GTH_RESP_LAPD_MESSAGE:    /* fall through */
  case GTH_RESP_MTP2_MESSAGE: {
    gth_print_timestamp();
    fprintf(stderr, "signalling job %s changed state to '%s'\n",
            gth_attribute_value(child, "id"),
            gth_attribute_value(child, "value"));
    break;
  }

  default:
    default_event_handler(data, resp);
    break;
  }
}

static void
connect_to_gth(GTH_api *api, struct Options *opts)
{
  int result;

  result = gth_connect(api, opts->hostname, opts->verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  default_event_handler = api->event_handler;
  api->event_handler = &event_handler;
}

// Entry point
int
main(int argc, char **argv)
{
  GTH_api api;
  int data_socket = -1;
  int i;
  int listen_port = 0;
  int listen_socket = -1;
  Start_function start_function = 0;

  // Check a couple of assumptions about type size.
  assert(sizeof(u32) == 4);
  assert(sizeof(u16) == 2);
  assert(sizeof(u8) == 1);

  win32_specific_startup();

  process_arguments(argv, argc, &options);
  connect_to_gth(&api, &options);
  read_hw_description(&api, options.hostname);
  if (!options.skip_l1_setup) {
    enable_l1(&api, options.channels, options.n_channels, options.monitoring);
  }

  listen_socket = gth_make_listen_socket(&listen_port);
  start_function = lookup_start_function(options.protocol);

  for (i = 0; i < options.n_channels; i++) {
    start_function(&api, options.channels + i, i, listen_port);
    if (i == 0) {
      data_socket = gth_wait_for_accept(listen_socket);
    }
  }

  fprintf(stderr, "capturing packets, press ^C to abort\n");
  convert_to_pcap(&api, data_socket, options);

  return 0;
}

// eof
