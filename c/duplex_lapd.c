//----------------------------------------------------------------------
// An example program. 
//
// Shows how to start duplex LAPD on a GTH, e.g. for making outgoing calls.

// Limitations:
//
//  Doesn't attempt any API error handling.
//  Doesn't check L1 status
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2010, Corelatus AB Stockholm
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

// N201=260 on the GTH.
#define MAX_SIGNAL_UNIT 300

typedef struct {
  unsigned short tag;
  unsigned char flags;
  unsigned char opcode;
  unsigned short reserved;
  char payload[MAX_SIGNAL_UNIT];
} GTH_lapd_rx_su;

typedef struct {
  unsigned short length;
  unsigned short tag;
  unsigned char flags;
  unsigned char opcode;
  unsigned short reserved;
  char payload[MAX_SIGNAL_UNIT];
} GTH_lapd_tx_su;

void usage() {
  fprintf(stderr, 
	  "duplex_lapd <GTH-IP> <span> <timeslot>"
	  "\n\nEnable ISDN LAPD on the specified timeslot\n\n");
  fprintf(stderr, "Typical use:\n");
  fprintf(stderr, "./duplex_lapd 172.16.1.10 1A 16\n");
  
  exit(-1);
}

// Start up L1 on the given span. It defaults to E1/doubleframe. We
// disable the TX pins since we're only listening.
static void enable_l1(GTH_api *api, const char* span) 
{
  int result;
  char pcm_name[20];

  // E1s carring ISDN LAPD normally use multiframe
  GTH_attribute attributes[] = { {"status", "enabled"}, 
				 {"framing", "multiframe"}
  };

  assert(sizeof(pcm_name) > (strlen(span) + strlen("pcm")));
  strncpy(pcm_name, "pcm", sizeof pcm_name);
  strncat(pcm_name, span, sizeof pcm_name);
  
  result = gth_set(api, pcm_name, attributes, 2);
  
  assert(result == 0);
}

// Start up duplex LAPD on the given timeslot
static int setup_lapd(GTH_api *api, 
		      const char* span,
		      const int timeslot)
{
  int listen_port = 0;
  int listen_socket = gth_make_listen_socket(&listen_port);
  int data_socket;
  int result;
  int sapi = 0;
  int tei = 0;
  char job_id[MAX_JOB_ID];

  if ( (timeslot < 1) || (timeslot > 31) ) {
    fprintf(stderr, "valid timeslots are 1--31, not %d. Aborting\n", timeslot);
    exit(-1);
  }

  result = gth_new_lapd_layer(api, 0, span, timeslot, "user", sapi, tei, 
			      job_id, api->my_ip, listen_port);
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
      die("LAPD data socket from GTH unexpectedly closed\n");

    count -= this_time;
    buf += this_time;
  }
}

static void dump_incoming_lapd(int data_socket)
{
  sleep(1000);
  // nyi
}

#define Q931_DL_ESTABLISH_REQUEST 5
#define Q931_DL_RELEASE_REQUEST   8

// Entry point 
int main(int argc, char** argv) 
{
  GTH_api api;
  int data_socket;
  int result;
  GTH_lapd_tx_su su;

  if (argc != 4) {
    usage();
  }

  win32_specific_startup();

  // Check a couple of assumptions about type size. 
  assert(sizeof(unsigned int) == 4);
  assert(sizeof(unsigned short) == 2);

  result = gth_connect(&api, argv[1]);
  assert(result == 0);

  enable_l1(&api, argv[2]);
  data_socket = setup_lapd(&api, argv[2], atoi(argv[3]));

  // To activate the link, send DL_EST_REQ 
  su.length = htons(6);
  su.tag = 0;
  su.flags = 0x21;
  su.opcode = Q931_DL_ESTABLISH_REQUEST;
  su.reserved = 0;

  result = send(data_socket, &su, ntohs(su.length) + sizeof(su.length), 0);
  assert(result == 8);

  fprintf(stderr, "lapd started, press ^C to abort\n");
  dump_incoming_lapd(data_socket);

  return 0; // not reached
}

// eof
