//----------------------------------------------------------------------
// An example program. Shows how to capture SS7 data off the wire (an E1/T1)
// with a GTH and save it to a libpcap file (or stdout) for further
// analysis with e.g. wireshark or tshark.
//
// The libpcap file format is documented here:
//
//   http://wiki.wireshark.org/Development/LibpcapFileFormat
//
// The GTH API is documented here:
//
//   http://www.corelatus.com/gth/api/
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

#ifdef WIN32
#include <winsock2.h>
#include <windows.h>
#else
#include <sys/socket.h>
#include <arpa/inet.h>
#endif // WIN32

#include "gth_win32_compat.h"
#include "gth_apilib.h"

// PCAP file format structures.
#pragma pack(1)
typedef struct {
  unsigned int magic;
  unsigned short major_version;
  unsigned short minor_version;
  unsigned int GMT_to_localtime;
  unsigned int sigfigs;
  unsigned int snaplen;
  unsigned int network;
} PACK_SUFFIX PCAP_global_header;

#pragma pack(1)
typedef struct {
  unsigned int ts_sec;
  unsigned int ts_us;
  unsigned int incl_len;
  unsigned int orig_len;
} PACK_SUFFIX PCAP_packet_header;

// GTH socket structure. An SS7 MTP-2 signal unit can't be more than
// 279 octets long according to Q.703.
#define MAX_SIGNAL_UNIT 300

// There are 31 useable timeslots in an E1
#define MAX_TIMESLOTS 31

// GTH 2.x has 16 E1/T1 inputs
// RAN probe has 64
#define MAX_PCMS 64

// How many MTP-2 channels can we run at the same time? (Also limited by
// hardware)
#define MAX_MTP2_CHANNELS 72

typedef struct {
  unsigned short tag;
  unsigned short flags;
  unsigned short timestamp_hi;
  unsigned int timestamp_lo;
  char payload[MAX_SIGNAL_UNIT];
} GTH_mtp2;

void usage() {
  fprintf(stderr,
	  "save_to_pcap <options> <GTH-IP> <channels> [<channels>...] <filename>"
	  "\n\nSave decoded MTP-2 signal units to a file in libpcap format, "
	  "\nsuitable for examining with wireshark, tshark or other network"
	  "\nanalyser software.\n"
	  "\n<options>: [-m] [-v] [-n <count>]"
	  "\n-m: tells the GTH that you are using a -20dB monitor point"
	  "\n-n <count>: rotate the output file after <count> packets, 0 means never"
	  "\n-v: print API commands and responses (verbose)"
	  "\n<GTH-IP> is the GTH's IP address or hostname"
	  "\n<channels> is a list of spans and timeslots:"
	  "\n  <span> [<span>...] <timeslot> [<timeslot>...]"
	  "\n  e.g. 1A 2A 1 2 3 will monitor timeslots 1, 2 and 3 on span 1A and 2A."
	  "\n<span> is the name of a span, e.g. '1A'"
	  "\n<timeslot> is a timeslot number, from 1 to 31"
	  "\n<filename> can be -, which means standard output.\n\n");
  fprintf(stderr,
	  "Examples:\n"
	  "./save_to_pcap 172.16.1.10 1A 2A 16 isup_capture.pcap\n"
	  "./save_to_pcap 172.16.1.10 1A 2A 3A 4A 1 2 3 4 isup_capture.pcap\n"
	  "./save_to_pcap 172.16.1.10 1A 2A 16 1B 5 6 7 8 capture.pcap\n"
	  "./save_to_pcap -m 172.16.1.10 1A 2A 16 isup_capture.pcap\n"
	  "./save_to_pcap -m -n 1000 172.16.1.10 1A 2A 16 isup_capture.pcap\n"
	  "./save_to_pcap 172.16.1.10 1A 2A 16 - | tshark -V -i - \n"
	  "./save_to_pcap 172.16.1.10 1A 2A 16 - | wireshark -k -i - \n"
	  "./save_to_pcap 172.16.1.10 1A 2A 16 \\\\.\\pipe\\isup_capture.1\n");

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
static int write_to_handle_or_file(void *buffer,
				   int length,
				   int items,
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

static HANDLE_OR_FILEPTR stdout_handle_or_file()
{
  return GetStdHandle(STD_OUTPUT_HANDLE);
}

#define fclose CloseHandle

#else
static HANDLE_OR_FILEPTR stdout_handle_or_file()
{
  return stdout;
}

#endif

static HANDLE_OR_FILEPTR open_windows_pipe(const char *filename)
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

static void open_file_for_writing(HANDLE_OR_FILEPTR *hf, const char *filename)
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

// Start up L1 on the given span. It defaults to E1/doubleframe. We
// disable the TX pins since we're only listening.
static void enable_l1(GTH_api *api, const char* span, const int monitoring)
{
  int result;
  char pcm_name[20];

  int n_attributes = (monitoring)?3:2;
  GTH_attribute attributes[] = { {"status", "enabled"},
				 {"tx_enabled", "false"},
				 {"monitoring", "true"}
  };

  assert(sizeof(pcm_name) > (strlen(span) + strlen("pcm")));
  strncpy(pcm_name, "pcm", sizeof pcm_name);
  strncat(pcm_name, span, sizeof pcm_name);

  result = gth_set(api, pcm_name, attributes, n_attributes);

  if (result != 0)
    die("Setting up L1 failed. (-v switch gives more information)");
}

// Start up MTP-2 monitoring on the given span and timeslot
static void monitor_mtp2(GTH_api *api,
			const char *span,
			int timeslot,
			int tag,
			int listen_port,
			int listen_socket
			)
{
  int result;
  char job_id[MAX_JOB_ID];

  result = gth_new_mtp2_monitor(api, tag, span, timeslot,
				job_id, api->my_ip, listen_port);
  if (result != 0)
    die("Setting up MTP2 monitoring failed. (-v gives more information)");

  return;
}

// Read exactly the requested number of bytes from the given descriptor
void read_exact(int fd, char* buf, size_t count) {
  size_t this_time;

  while (count > 0) {
    this_time = recv(fd, buf, count, 0);
    if (this_time <= 0)
      die("MTP-2 data socket from GTH unexpectedly closed\n");

    count -= this_time;
    buf += this_time;
  }
}

// Write a PCAP global header
static void write_pcap_header(HANDLE_OR_FILEPTR file)
{
  int result;
  PCAP_global_header header;

  // The pcap file is native-endian, i.e. wireshark uses the magic value
  // to figure out if it was created on a little-endian or big-endian machine.
  header.magic = 0xa1b2c3d4;
  header.major_version = 2;
  header.minor_version = 4;
  header.GMT_to_localtime = 0;
  header.sigfigs = 0;
  header.snaplen = 65535;
  header.network = 140;   // 140 == MTP-2

  result = fwrite((void*)&header, sizeof header, 1, file);

  if (result != 1) {
    die("Unable to write PCAP header. (Is the file writeable?)");
  }

  return;
}

// Write a PCAP per-packet header
static void write_packet_header(HANDLE_OR_FILEPTR file,
				unsigned int timestamp_hi,
				unsigned int timestamp_lo,
				int length)
{
  PCAP_packet_header pcap_header;
  unsigned long long ts_sec;
  unsigned long long ts_us;
  int result;

  assert(sizeof ts_sec == 8);

  ts_us = ntohs(timestamp_hi);
  ts_us <<= 32;
  ts_us += ntohl(timestamp_lo);

  ts_sec = ts_us / 1000;
  ts_us = (ts_us % 1000) * 1000;

  pcap_header.ts_sec = (unsigned int)ts_sec;
  pcap_header.ts_us =  (unsigned int)ts_us;
  pcap_header.incl_len = length;
  pcap_header.orig_len = length;

  result = fwrite(&pcap_header, sizeof pcap_header, 1, file);
  if (result != 1) {
    die("Unable to write packet to the given file. (Is it writeable?)");
  }
}

static void write_packet_payload(HANDLE_OR_FILEPTR file, char *payload,
				 int length) {
  int result;

  result = fwrite(payload, length, 1, file);
  if (result != 1)
    die("Unable to write packet to the given file. (Is it writeable?)");
}

#define MAX_FILENAME 100

// Loop forever, converting the incoming GTH data to libpcap format
static void convert_to_pcap(int data_socket,
			    const char *base_name,
			    const int n_sus_per_file
			    )
{
  unsigned short length;
  GTH_mtp2 signal_unit;
  int su_count;
  int file_number = 1;
  HANDLE_OR_FILEPTR file;
  int write_to_stdout = 0;
  const char pipe_prefix[] = "\\\\.\\pipe\\";
  int write_to_pipe;

  write_to_stdout = (strcmp(base_name, "-") == 0);
  write_to_pipe = (strncmp(pipe_prefix, base_name, strlen(pipe_prefix)) == 0);

  while (1) {
    char filename[MAX_FILENAME];

    if (!write_to_stdout && !write_to_pipe) {
      snprintf(filename, MAX_FILENAME, "%s.%d",
	       base_name, file_number);
      open_file_for_writing(&file, filename);
      fprintf(stderr, "saving to file %s\n", filename);
    }
    else if (write_to_stdout) {
      file = stdout_handle_or_file();
      fprintf(stderr, "saving capture to stdout\n");
    }
    else {
      fprintf(stderr, "saving capture to a windows named pipe\n");
      file = open_windows_pipe(base_name);
    }

    write_pcap_header(file);
    file_number++;
    su_count = 0;

    while ( write_to_stdout
	    || write_to_pipe
	    || n_sus_per_file == 0
	    || (su_count++ < n_sus_per_file) ) {
      read_exact(data_socket, (void*)&length, sizeof length);
      length = ntohs(length);
      assert(length <= sizeof signal_unit);
      read_exact(data_socket, (void*)&signal_unit, length);

      length -= (signal_unit.payload - (char*)&(signal_unit.tag));

      write_packet_header(file,
			  signal_unit.timestamp_hi, signal_unit.timestamp_lo,
			  length);

      write_packet_payload(file, signal_unit.payload, length);

      if (write_to_stdout)
	{
	  fflush(stdout);
	}
    }

    fclose(file);
  }
}

static int is_span_name(char *arg){
  if (strstr(arg, "A")) return 1;
  if (strstr(arg, "B")) return 1;
  if (strstr(arg, "C")) return 1;
  if (strstr(arg, "D")) return 1;
  return 0;
}

typedef struct {
  char *pcm;
  int timeslot;
} Channels_t;

// Entry point
int main(int argc, char** argv)
{
  GTH_api api;
  int current_arg;
  int data_socket = -1;
  int result;
  int monitoring = 0;
  int verbose = 0;
  Channels_t channels[MAX_MTP2_CHANNELS];
  int i;
  char *pcms[MAX_PCMS];
  int n_channels = 0;
  int n_pcms = 0;
  int n_sus_per_file = 0;
  int timeslot = 0;
  int listen_port = 0;
  int listen_socket = -1;
  int tag;

  // Check a couple of assumptions about type size.
  assert(sizeof(unsigned int) == 4);
  assert(sizeof(unsigned short) == 2);

  win32_specific_startup();

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'm': monitoring = 1; break;

    case 'v': verbose = 1; break;

    case 'n':
      if (argc < 3) {
	usage();
      } else {
	n_sus_per_file = atoi(argv[2]);
	argc--;
	argv++;
      }
      break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc < 4) {
    usage();
  }

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  current_arg = 2;
  while (current_arg < argc - 1){
    if (is_span_name(argv[current_arg])){
      if (timeslot){
	// We are starting a new pcm group
	n_pcms = 0;
	timeslot = 0;
      }
      pcms[n_pcms] = argv[current_arg];
      n_pcms++;
      enable_l1(&api, argv[current_arg], monitoring);
    }
    else{
      timeslot = atoi(argv[current_arg]);
      if ( (timeslot < 1) || (timeslot > 31) ) {
	fprintf(stderr, "Valid timeslots are 1--31, not %d. Abort.\n", timeslot);
	exit(-1);
      }
      if (!n_pcms){
	die("Timeslot given without previous pcm.");
      }
      for (i = 0; i < n_pcms; i++){
	if (n_channels >= MAX_MTP2_CHANNELS)
	  die("Attempted to start too many signalling channels. Abort.");

	channels[n_channels].pcm = pcms[i];
	channels[n_channels].timeslot = timeslot;
	n_channels++;
      }
    }
    current_arg++;
  }

  if (!n_channels){
    die("No timeslots given (or, perhaps, no output filename given).");
  }

  tag = 0;
  listen_socket = gth_make_listen_socket(&listen_port);
  for (i = 0; i < n_channels; i++){
    monitor_mtp2(&api, channels[i].pcm, channels[i].timeslot,
		 tag, listen_port, listen_socket);
    if (!tag) {
      data_socket = gth_wait_for_accept(listen_socket);
    }
    tag++;
  }

  fprintf(stderr, "capturing packets, press ^C to abort\n");
  convert_to_pcap(data_socket, argv[current_arg], n_sus_per_file);

  return 0; // not reached
}

// eof
