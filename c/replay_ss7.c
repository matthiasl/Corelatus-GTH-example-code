//----------------------------------------------------------------------
// An example program.
//
// Replays SS7 packets on an E1, either from an MTP-2 or an MTP-3 capture.
//
// The packets are read from a PCapNG file, i.e. Wireshark, though we
// make some assumptions about the file layout:
//
//    - The file must come from a machine with the same endianness as
//      the one running this program (but you can convert using 'tshark')
//
//    - The file must contain either MTP-2 or MTP-3 packets without
//      pseudo-headers. 'editcap' can be used to convert other file types.
//
//    - Packet timing is ignored, i.e packets are played back as fast
//      as possible.
//
//    - All packets are played back. If you want to filter, use 'tshark'
//      to pre-filter the file.
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2019, Corelatus AB Stockholm
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
#include "pcap_structs.h"

static void usage(void) {
  fprintf(stderr,
	  "replay_ss7: %s build_hostname: %s\n\n"

	  "replay_ss7 [-vf] <GTH-IP> <pcap-filename> <span> <timeslot>\n\n"
	  "Transmit SS7 packets on the specified timeslot.\n\n"
	  "-v: print the API commands and responses (verbose)\n"
	  "-f: assume the PCap file doesn't include the FCS (CRC)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
          "<pcap-filename> is the name of a PCAP-NG file with SS7 packets\n"
	  "<span> is the E1/T1 interface, e.g. '1A'\n"
	  "<timeslot> is the timeslot, 1--31\n",
	  git_head, build_hostname);
  fprintf(stderr, "Typical use:\n");
  fprintf(stderr, "./replay_ss7 172.16.1.10 isup.pcap 1A 16\n");

  exit(-1);
}

// The PCap file we're reading
static FILE *pcap;

// The link type according to the PCap file.
static u16 link_type;

// How many empty bytes go before the read packet?
static int link_type_offset;

// How many bytes do we discard from the end of each read packet?
static int link_type_adjustment;

static int setup_transmitter(GTH_api *api,
                             const char *span,
                             const int timeslot)
{
  int listen_port = 0;
  int listen_socket = gth_make_listen_socket(&listen_port);
  int data_socket;
  int result;
  char job_id[MAX_JOB_ID];

  result = gth_new_fr_layer(api, span, &timeslot, 1,
			      job_id, api->my_ip, listen_port);
  if (result != 0) {
    die("starting FR failed. (re-run with -v for more information)");
  }

  data_socket = gth_wait_for_accept(listen_socket);

  return data_socket;
}

static void checked_fread(void *buf, size_t size) {
  size_t result;

  result = fread(buf, size, 1, pcap);
  if (result != 1) die("short read from PCAP file");
}

static void open_pcap_file(const char *filename) {
  PCap_NG_shb shb;
  int result;
  char *classic_pcap = "File looks like it's in classic PCAP format.\n"
    "Convert it with 'tshark -r classic.pcap -w modern.pcap'\n";

  pcap = fopen(filename, "r");
  if (!pcap) die("unable to open the given .pcap file");

  checked_fread(&shb, sizeof shb);

  switch (shb.type) {
  case 0x0a0d0d0a: /* correct, do nothing */ break;
  case 0xd4c3b2a1: /* fall through */
  case 0xa1b2c3d4: die(classic_pcap); break;
  default: die("this doesn't look like a PCAP-ng file");
  }

  if (shb.byte_order_magic != 0x1A2B3C4D)
    die("PCAP file endianness different from this machine\n"
        "Use Wireshark or tshark or editcap to convert it.\n");

  if (shb.major_version != 1 || shb.minor_version > 0)
    die("PCAP file version is too new; more than 1.0");

  result = fseek(pcap, shb.block_total_length - sizeof(shb), SEEK_CUR);
  if (result != 0)
    die("PCAP file seek failed");
}

static int length_indicator(int length) {
  return (length > 63)?63:length;
}

#define MAX_PACKET 300

static int read_epb(char *packet) {
  int result;
  PCap_NG_epb epb;

  result = fread(&epb, sizeof epb, 1, pcap);
  if (result != 1)
    return 0;

  // Set up fake MTP-2 in case capture file isn't MTP-2.
  packet[0] = 0;
  packet[1] = 1;
  packet[2] = length_indicator(epb.packet_len);

  if (epb.captured_len != epb.packet_len)
    die("PCAP file contains truncated packets");

  if (epb.captured_len + link_type_offset > MAX_PACKET)
    die("PCAP file contains unexpectedly long packets");

  result = fread(packet + link_type_offset, epb.captured_len, 1, pcap);
  if (result != 1)
    die("PCAP file ended mid-packet");

  result = fseek(pcap, epb.block_total_length - sizeof(epb) - epb.captured_len,
                 SEEK_CUR);
  if (result != 0)
    die("seek failed");

  return epb.captured_len - link_type_adjustment + link_type_offset;
}

static void read_idb(void) {
  int result;
  PCap_NG_idb idb;

  result = fread(&idb, sizeof idb, 1, pcap);
  if (result != 1)
    die("read failed in the middle of an IDB");

  result = fseek(pcap, idb.block_total_length - sizeof(idb), SEEK_CUR);
  if (result != 0)
    die("seek failed in the middle of an IDB");

  if (link_type != 0 && link_type != idb.link_type)
    die("capture file contains conflicting IDBs; can't work with that");

  fprintf(stderr, "Found link type %d in capture file\n", idb.link_type);

  switch (idb.link_type) {
  case LINK_TYPE_MTP2:
    link_type_offset = 0;
    link_type_adjustment += 2;   // Assume file contains FCS (CRC). -f switch.
    break;

  case LINK_TYPE_MTP3:
    link_type_offset = 3;
    link_type_adjustment = 0;
    break;

  default:
    die("Supported types are 141 (MTP-2) and 142 (MTP-3). See also 'editcap'.");
  }

  link_type = idb.link_type;
}

// Returns the packet length
static int read_packet_from_pcap(char *packet)
{
  PCap_NG_block block;
  int result;

  // Process blocks until we find an EPB
  for (;;) {
    result = fread(&block, sizeof block, 1, pcap);
    if (result != 1) return 0;
    result = fseek(pcap, -sizeof(block), SEEK_CUR);
      if (result != 0) return 0;

    switch (block.type) {
    case PCAPNG_BLOCK_TYPE_EPB:
      return read_epb(packet);
      break;

    case PCAPNG_BLOCK_TYPE_IDB:
      read_idb();
      break;

    default:
      result = fseek(pcap, block.block_total_length, SEEK_CUR);
      if (result != 0)
        die("PCAP file seek failed");
    }
  }

  // never reached
}

#define TX_AND_FISU_OPCODE 3

static void copy_packets(int data_socket)
{
  // Packet starts off with a six-byte header which is
  // length:16, opcode:16, reserved:16
  char packet[MAX_PACKET + 6] = {0,0, 0, TX_AND_FISU_OPCODE, 0,0};
  int length;
  int result;
  int n_bytes = 0;

  for (;;) {
    length = read_packet_from_pcap(packet + 6);
    if (length < 5) break;
    n_bytes += length;
    length += 4;    // length = pcap_length + 2(opcode) + 2(padding)
    packet[0] = (length >> 8);
    packet[1] = (length & 255);
    result = send(data_socket, packet, length + 2, 0);
    assert(result == length + 2);
  }

  fprintf(stderr, "packet bytes replayed: %d. Sleeping to drain buffers.\n",
          n_bytes);

  sleep_seconds(1 + n_bytes / 7000);
}

// Entry point
int main(int argc, char** argv)
{
  GTH_api api;
  int data_socket;
  int result;
  int verbose = 0;

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'v': verbose = 1; break;
    case 'f': link_type_adjustment -= 2; break;
    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc != 5)
    {
      usage();
    }

  win32_specific_startup();

  // Check a couple of assumptions about type size.
  assert(sizeof(unsigned int) == 4);
  assert(sizeof(unsigned short) == 2);

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  data_socket = setup_transmitter(&api, argv[3], checked_atoi(argv[4]));
  open_pcap_file(argv[2]);
  copy_packets(data_socket);

  return 0; // not reached
}

// eof
