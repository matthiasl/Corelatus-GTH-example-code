//----------------------------------------------------------------------
// Minimal program to forward an entire E1/T1 over UDP and then dump to a file
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2013, Corelatus AB Stockholm
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
//
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <assert.h>
#include <string.h>

#ifdef WIN32
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <unistd.h>
#include <arpa/inet.h>
#endif

#include "gth_win32_compat.h"
#include "gth_apilib.h"

static void usage()
{
  fprintf(stderr,
	  "wide_recorder [-v] <GTH-IP> <span> <filename>\n\n"
	  "Save bit-exact representing an entire E1/T1 to a file\n\n"
	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<span> is the E1/T1 interface, e.g. '1A'\n"
	  "<filename> is the filename to save to\n\n");

  fprintf(stderr, "Typical use:\n");
  fprintf(stderr, "./wide_recorder 172.16.1.10 13 capture_file.raw\n\n");

  exit(-1);
}

typedef unsigned int   u32;
typedef unsigned short u16;

// GTH will normally sends 32 frames, i.e. about 1000 bytes, per packet
#define WIDE_RECORDER_PAYLOAD_BYTES 1600

// See the GTH API manual for more information. The header is always sent
// in network byte order ("big endian"). We adjust the field endianness
// when the fields are used.
struct Wide_recorder_header {
  u16 tag;
  u16 res;
  u16 seq;
  u16 timestamp_hi;
  u32 timestamp_low;
};

struct Wide_recorder_packet {
  struct Wide_recorder_header header;
  char payload[WIDE_RECORDER_PAYLOAD_BYTES];
};

static void wide_record_a_file(GTH_api *api,
			       const char *span,
			       const char *filename)
{
  int data_socket;
  size_t octet_count;
  size_t packet_count = 0;
  u16 seq = 0;
  char job_id[MAX_JOB_ID];
  int result;
  FILE* file = 0;
  struct Wide_recorder_packet packet;
  int n_seq_errors = 0;

  // Will fail if the compiler has added unexpected padding to the
  // structure, or if u16/u32 aren't the expected size.
  assert(sizeof(packet.header) == 12);
  assert(sizeof(packet) == WIDE_RECORDER_PAYLOAD_BYTES + sizeof(packet.header));

  fopen_s(&file, filename, "wb");
  if (file == 0) {
    fprintf(stderr, "unable to open %s, aborting\n", filename);
    exit(-1);
  }

  data_socket = gth_new_wide_recorder(api, span, job_id);
  if (data_socket < 0) {
    die("unable to start a <wide_recorder>. -v gives more information");
  }

  fprintf(stderr, "started recording. Press ^C to end.\n");

  while ( (octet_count = recv(data_socket, (void*)&packet, sizeof packet, 0))) {

    packet.header.seq = ntohs(packet.header.seq);
    if (packet.header.seq != seq) {
      n_seq_errors++;
    }
    seq = packet.header.seq + 1;

    result = fwrite(packet.payload, octet_count, 1, file);

    if (result != 1) {
      die("Writing to the output file failed. Giving up.");
    }

    if (packet_count++ % 100 == 0) {
      fprintf(stderr, "seq: %d packet_count: %zu seq_errors: %u\n",
	      seq, packet_count, n_seq_errors);
    }
  }
  closesocket(data_socket);
  fclose(file);
}

#define MAX_L1_STATUS 100
static void check_layer_1(GTH_api *api, const char *pcm_name) {
  char buffer[MAX_L1_STATUS];
  int result;

  result = gth_query_resource_attribute(api, pcm_name, "status",
					buffer, MAX_L1_STATUS);
  assert(result == 0);

  if (strcmp(buffer, "OK") && strcmp(buffer, "RAI")) {
    fprintf(stderr,
	    "Warning: L1 status of %s is '%s', data is most likely useless\n",
	    pcm_name, buffer);
  }
}

// Entry point
int main(int argc, char** argv)
{
  int result;
  GTH_api api;
  char pcm_name[20];
  int verbose = 0;

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'v': verbose = 1; break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc != 4) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("unable to connect to the GTH");
  }

  assert(sizeof(pcm_name) > (strlen("pcm") + strlen(argv[2])));
  strncpy(pcm_name, "pcm", sizeof pcm_name);
  strncat(pcm_name, argv[2], sizeof pcm_name);

  check_layer_1(&api, pcm_name);

  wide_record_a_file(&api, argv[2], argv[3]);

  gth_bye(&api);

  fprintf(stderr, "all done\n");

  return 0;
}
