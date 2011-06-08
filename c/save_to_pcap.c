//----------------------------------------------------------------------
// An example program. Shows how to capture SS7 data off the wire with
// a GTH and save it to a libpcap file (or stdout) for further
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

typedef struct {
  unsigned short tag;
  unsigned short flags;
  unsigned short timestamp_hi;
  unsigned int timestamp_lo;
  char payload[MAX_SIGNAL_UNIT];
} GTH_mtp2;

void usage() {
  fprintf(stderr,
	  "save_to_pcap [-m] [-v] <GTH-IP> <span> <span> <ts> [<ts> ...] <filename>"
	  "\n\nSave decoded MTP-2 signal units from the same timeslot"
	  "\non two spans to a file in libpcap format, suitable for"
	  "\nexamining with wireshark, tshark or other network"
	  "\nanalyser software.\n"
	  "\n-m: tells the GTH that you are using a -20dB monitor point"
	  "\n-v: print API commands and responses (verbose)"
	  "\n<GTH-IP> is the GTH's IP address or hostname"
	  "\n<span> is the name of a span, e.g. '1A'"
	  "\n<ts> is a timeslot number, from 1 to 31"
	  "\n<filename> can be -, which means standard output.\n\n");
  fprintf(stderr, "Typical ways to use this program:\n");
  fprintf(stderr, "./save_to_pcap 172.16.1.10 1A 2A 16 isup_capture.pcap\n");
  fprintf(stderr, "./save_to_pcap -m 172.16.1.10 1A 2A 16 isup_capture.pcap\n");
  fprintf(stderr, "./save_to_pcap 172.16.1.10 1A 2A 16 - | tshark -V -i - \n");
  fprintf(stderr, "./save_to_pcap 172.16.1.10 1A 2A 16 - | wireshark -k -i - \n");

  exit(-1);
}

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

// Start up MTP-2 monitoring on the same timeslot of the given spans.
static int monitor_mtp2(GTH_api *api,
			const char *span1,
			const char *span2,
			int *timeslots,
			int n_timeslots)
{
  int listen_port = 0;
  int listen_socket = gth_make_listen_socket(&listen_port);
  int data_socket;
  int result;
  int tag = 0;
  char job_id[MAX_JOB_ID];

  for (tag = 0; tag < n_timeslots; tag++) {
    int timeslot;

    timeslot = *timeslots++;
    result = gth_new_mtp2_monitor(api, tag, span1, timeslot,
				  job_id, api->my_ip, listen_port);
    if (result != 0)
      die("Setting up MTP2 monitoring failed. (-v gives more information)");

    result = gth_new_mtp2_monitor(api, tag, span2, timeslot,
				  job_id, api->my_ip, listen_port);
    if (result != 0)
      die("Setting up MTP2 monitoring failed. (-v gives more information)");
  }

  data_socket = gth_wait_for_accept(listen_socket);

  return data_socket;
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
static void write_pcap_header(FILE* file)
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

  if (result != 1)
    die("Unable to write PCAP header. (Is the file writeable?)");

  return;
}


#define MAX_FILENAME 100
#define SUS_PER_FILE 1000

// Loop forever, converting the incoming GTH data to libpcap format
//
// Saves SUS_PER_FILE signal units to a file and then moves to the next file.
static void convert_to_pcap(int data_socket,
			    const char *base_name,
			    const char *span1,
			    const char *span2,
			    const int timeslots[],
			    const int n_timeslots
			    )
{
  unsigned short length;
  int result;
  GTH_mtp2 signal_unit;
  PCAP_packet_header pcap_header;
  unsigned long long ts_sec;
  unsigned long long ts_us;
  int su_count;
  int file_number = 1;
  FILE *file;
  int write_to_stdout = (strcmp(base_name, "-") == 0);

  assert(sizeof ts_sec == 8);

  while (1) {
    char filename[MAX_FILENAME];

    if (!write_to_stdout) {
      snprintf(filename, MAX_FILENAME, "%s_%s_%s.%d",
	       base_name, span1, span2, file_number);
      fopen_s(&file, filename, "wb");
      if (file == 0) {
	fprintf(stderr, "unable to open %s for writing. Aborting.\n", filename);
	exit(-1);
      }
      fprintf(stderr, "saving to file %s\n", filename);
      write_pcap_header(file);
    }
    else {
      file = stdout;
      fprintf(stderr, "saving capture to stdout\n");
    }

    file_number++;
    su_count = 0;

    while ( (file == stdout) || (su_count++ < SUS_PER_FILE) ) {
      read_exact(data_socket, (void*)&length, sizeof length);
      length = ntohs(length);
      assert(length <= sizeof signal_unit);
      read_exact(data_socket, (void*)&signal_unit, length);

      ts_us = ntohs(signal_unit.timestamp_hi);
      ts_us <<= 32;
      ts_us += ntohl(signal_unit.timestamp_lo);

      ts_sec = ts_us / 1000;
      ts_us = (ts_us % 1000) * 1000;

      pcap_header.ts_sec = (unsigned int)ts_sec;
      pcap_header.ts_us =  (unsigned int)ts_us;
      pcap_header.incl_len = length;
      pcap_header.orig_len = length;
      assert(ntohs(signal_unit.tag) < n_timeslots);

      result = fwrite(&pcap_header, sizeof pcap_header, 1, file);
      if (result != 1)
	die("Unable to write packet to the given file. (Is it writeable?)");

      result = fwrite(signal_unit.payload, length, 1, file);
      if (result != 1)
	die("Unable to write packet to the given file. (Is it writeable?)");

      if (write_to_stdout)
	{
	  fflush(stdout);
	}
    }

    fclose(file);
  }
}

// Entry point
int main(int argc, char** argv)
{
  GTH_api api;
  int data_socket;
  int result;
  int monitoring = 0;
  int verbose = 0;
  int timeslots[MAX_TIMESLOTS];
  int n_timeslots = 0;
  char *span1;
  char *span2;

  // Check a couple of assumptions about type size.
  assert(sizeof(unsigned int) == 4);
  assert(sizeof(unsigned short) == 2);

  win32_specific_startup();

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'm': monitoring = 1; break;

    case 'v': verbose = 1; break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc < 6) {
    usage();
  }

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  span1 = argv[2];
  span2 = argv[3];

  enable_l1(&api, span1, monitoring);
  enable_l1(&api, span2, monitoring);

  argv += 4;
  argc -= 4;

  for (n_timeslots = 0; argc > 1; n_timeslots++, argc--) {
    int timeslot = atoi(*argv++);
    timeslots[n_timeslots] = timeslot;
    if ( (timeslot < 1) || (timeslot > 31) ) {
      fprintf(stderr, "valid timeslots are 1--31, not %d. Abort.\n", timeslot);
      exit(-1);
    }
  }

  data_socket = monitor_mtp2(&api, span1, span2, timeslots, n_timeslots);
  fprintf(stderr, "capturing packets, press ^C to abort\n");
  convert_to_pcap(data_socket, *argv, span1, span2, timeslots, n_timeslots);

  return 0; // not reached
}

// eof
