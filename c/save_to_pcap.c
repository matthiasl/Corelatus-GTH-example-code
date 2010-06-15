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
// $Id: save_to_pcap.c,v 1.15 2010-06-15 13:16:28 matthias Exp $
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

typedef struct {
  unsigned short tag;
  unsigned short flags;
  unsigned short timestamp_hi;
  unsigned int timestamp_lo;
  char payload[MAX_SIGNAL_UNIT];
} GTH_mtp2;

void usage() {
  fprintf(stderr, 
	  "save_to_pcap <GTH-IP> <span> <span> <timeslot> <filename>"
	  "\n\nSave decoded MTP-2 signal units from the same timeslot"
	  "\non two spans to a file in libpcap format, suitable for"
	  "\nexamining with wireshark, tshark or other network"
	  "\nanalyser software.\n"
	  "\n<filename> can be -, which means standard output.\n\n");
  fprintf(stderr, "Typical ways to use this program:\n");
  fprintf(stderr, "./save_to_pcap 172.16.1.10 1A 2A 16 isup_capture.pcap\n");
  fprintf(stderr, "./save_to_pcap 172.16.1.10 1A 2A 16 - | tshark -V -i - \n");
  fprintf(stderr, "./save_to_pcap 172.16.1.10 1A 2A 16 - | wireshark -k -i - \n");
  
  exit(-1);
}

#define MAX_API_COMMAND 1000

// Start up L1 on the given span. It defaults to E1/doubleframe. We
// disable the TX pins since we're only listening.
static void enable_l1(GTH_api *api, const char* span) 
{
  int result;
  char pcm_name[20];
  GTH_attribute attributes[] = { {"status", "enabled"}, 
				 {"tx_enabled", "false"},
				 // uncomment the line below if you 
				 // want to use a -20dB monitor point
				 // {"monitoring", "true"}
  };

  assert(sizeof(pcm_name) > (strlen(span) + strlen("pcm")));
  strncpy(pcm_name, "pcm", sizeof pcm_name);
  strncat(pcm_name, span, sizeof pcm_name);
  
  result = gth_set(api, pcm_name, attributes, 2);
  
  assert(result == 0);
}

// Start up MTP-2 monitoring on the same timeslot of the given spans.
static int monitor_mtp2(GTH_api *api, 
			const char* span1,
			const char* span2,
			const int timeslot)
{
  int listen_port = 0;
  int listen_socket = gth_make_listen_socket(&listen_port);
  int data_socket;
  int result;
  char job_id[MAX_JOB_ID];

  if ( (timeslot < 1) || (timeslot > 31) ) {
    fprintf(stderr, "valid timeslots are 1--31, not %d. Aborting\n", timeslot);
    exit(-1);
  }

  result = gth_new_mtp2_monitor(api, 1, span1, timeslot, job_id, api->my_ip,
				listen_port);
  assert(result == 0);

  result = gth_new_mtp2_monitor(api, 2, span2, timeslot, job_id, api->my_ip,
				listen_port);
  assert(result == 0);

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
  assert(result == 1);

  return;
}


#define MAX_FILENAME 100
#define SUS_PER_FILE 1000

// Loop forever, converting the incoming GTH data to libpcap format
//
// Saves SUS_PER_FILE signal units to a file and then moves to the next file.
static void convert_to_pcap(int data_socket, 
			    const char* base_name) 
{
  unsigned short length;
  int result;
  GTH_mtp2 signal_unit;
  PCAP_packet_header pcap_header;
  unsigned long long ts_sec;
  unsigned long long ts_us;
  int su_count;
  int file_number = 1;
  FILE* file;

  assert(sizeof ts_sec == 8);

  while (1) {
    char filename[MAX_FILENAME];

    if (strcmp(base_name, "-") != 0) 
      {

	snprintf(filename, MAX_FILENAME, "%s.%d", base_name, file_number);
	fopen_s(&file, filename, "wb");
	if (file == 0) {
	  fprintf(stderr, "unable to open %s for writing. Abort.\n", filename);
	  exit(-1);
	}
	fprintf(stderr, "saving capture to file %s\n", filename);
      } 
    else 
      {
	fprintf(stderr, "saving capture to stdout\n");
	file = stdout;
      }
    write_pcap_header(file);
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

      result = fwrite(&pcap_header, sizeof pcap_header, 1, file);
      assert(result == 1);
      result = fwrite(signal_unit.payload, length, 1, file);
      assert(result == 1);
      if (file == stdout) 
	{
	  fflush(file);
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

  if (argc != 6) {
    usage();
  }

  win32_specific_startup();

  // Check a couple of assumptions about type size. 
  assert(sizeof(unsigned int) == 4);
  assert(sizeof(unsigned short) == 2);

  result = gth_connect(&api, argv[1]);
  assert(result == 0);

  enable_l1(&api, argv[2]);
  enable_l1(&api, argv[3]);
  data_socket = monitor_mtp2(&api, argv[2], argv[3], atoi(argv[4]));
  fprintf(stderr, "capturing packets, press ^C to abort\n");
  convert_to_pcap(data_socket, argv[5]);

  return 0; // not reached
}

// eof
