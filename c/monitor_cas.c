//----------------------------------------------------------------------
// Minimal program to demonstrate how CAS monitoring is set up on
// a Corelatus GTH.
//
// Doesn't attempt any error handling.
// Doesn't keep track of L1 status
// Doesn't log L1 errors and status changes
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
//----------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#ifdef WIN32
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#endif // WIN32

#include <assert.h>

#include "gth_win32_compat.h"
#include "gth_apilib.h"

#define PROTOCOL_MFC 0xe020
#define PROTOCOL_LINESIG 0xe040
#define PROTOCOL_MASK 0xe0e0

static void usage() 
{
  fprintf(stderr, 
	  "monitor_cas [-v] <GTH-IP>\n\n"
	  "Monitor an E1, pcm2A, for CAS signalling, print the signalling\n\n"

	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n\n"

	  "Typical use:\n"
	  "monitor_cas 172.16.1.10\n");
  
  exit(-1);
}

// Enable listen-only operation on pcm2A and pcm2C.
//
// We use listen-only because 
//      (a) that's what we want to do
//  and (b) pcm2C can't do transmit at all (no pins for it)
//
// If we were getting a -20dB signal, we'd want to use 'monitoring=true' 
// instead.
//
// We use both pcm2A and pcm2C because we assume this is being used
// with a crossed cable from another port. If it's an E1 crossed
// cable, it'll appear on pcm2A, if it's a straight cable, it'll appear
// on pcm2C.
//
static void layer1(GTH_api *api) 
{
  fprintf(stderr, "setting up layer 1 (pcm2A, 2B, 2C, 2D)\n");
  gth_set_single(api, "pcm2A", "tx_enabled", "false");
  gth_set_single(api, "pcm2B", "tx_enabled", "false");
  gth_set_single(api, "pcm2C", "tx_enabled", "false");
  gth_set_single(api, "pcm2D", "tx_enabled", "false");
}

// Read the data that comes in on the signalling socket. The format
// is documented in the GTH API manual, in the 
// section "new_cas_r2_mfc_monitor". Each packet is always 0x0e octets long,
// not including the length header.
static void read_loop(int s) 
{
  short length;
  int result;
  
#pragma pack(1)
  struct mfc_detection {
    unsigned short tag;
    unsigned short bitfield;
    unsigned short timestamp_hi;
    unsigned int timestamp_lo;
    unsigned short reserved;
    unsigned char type;
    unsigned char digit;
  } PACK_SUFFIX;

  struct mfc_detection md;

  fprintf(stderr, "Waiting for tone detections\n");

  assert(sizeof md == 0x0e);
  assert(sizeof length == 2);

  for (;;) {
    result = recv(s, (void*)&length, 2, 0);
    if (result != 2) {
      die("short read in read_loop. Aborting.");
    }

    length = ntohs(length);
    assert(length == 0x0e);

    result = recv(s, (void*)&md, sizeof md, 0);
    if (result != 0x0e) {
      die("short read in read_loop. Aborting.");
    }

    md.tag = ntohs(md.tag);
    md.bitfield = ntohs(md.bitfield);
    
    switch (md.bitfield & PROTOCOL_MASK) {
    case  PROTOCOL_MFC:
      printf("MFC tone. tag=%d type=%d digit=%d\n",
	     md.tag, md.type, md.digit);
      break;

    case PROTOCOL_LINESIG:
      printf("line signalling bit change. tag=%d channel=%2d bits=%x\n",
	     md.tag, md.type, md.digit);
      break;

    default:
      die("unexpected signalling data received. Aborting.\n");
    }
  }
}

// Enable CAS MFC detection on timeslot 1 of both pcm2A and pcm2C.
// We use different tags.
//
// Same thing for CAS line signalling.
static void layer2(GTH_api *api) 
{
  int listen_port = 0;
  int listen_socket = 0;
  int data_socket = 0;
  int result;
  char job_id[MAX_JOB_ID];

  fprintf(stderr, 
	  "setting up layer 2, MFC on timeslot 1, line signalling on 16\n");

  listen_socket = gth_make_listen_socket(&listen_port);
  if (listen_socket < 0) {
    die("Unable to create a listen() socket. Giving up.\n");
  }

  result = gth_new_cas_r2_mfc_detector(api, 1, "2A", 1, job_id, api->my_ip, 
				       listen_port);
  if (result != 0) {
    die("unable to set up MFC detection (use -v for more information)");
  }
  result = gth_new_cas_r2_mfc_detector(api, 2, "2C", 1, job_id, api->my_ip, 
				       listen_port);
  if (result != 0) {
    die("unable to set up MFC detection (use -v for more information)");
  }

  result = gth_new_cas_r2_linesig_monitor(api, 3, "2A", 16, job_id, api->my_ip, 
					  listen_port);
  if (result != 0) {
    die("unable to set up linesig detection (use -v for more information)");
  }

  result = gth_new_cas_r2_linesig_monitor(api, 4, "2C", 16, job_id, api->my_ip, 
					  listen_port);
  if (result != 0) {
    die("unable to set up linesig detection (use -v for more information)");
  }

  fprintf(stderr, "waiting for GTH to connect to our signalling socket\n");
  data_socket = gth_wait_for_accept(listen_socket);
  if (data_socket < 0) {
    die("unable to accept() the signalling socket (use -v for details)");
  }

  read_loop(data_socket);
}

// Entry point 
int main(int argc, char** argv) 
{
  int result;
  GTH_api api;
  int verbose = 0;

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'v': verbose = 1; break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc != 2) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  layer1(&api);
  layer2(&api);
  fprintf(stderr, "all done\n");

  return 0;
}
