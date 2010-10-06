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
// $Id: duplex_lapd.c,v 1.1 2010-10-06 23:42:25 matthias Exp $
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

// Opcodes from the GTH API manual
#define Q921_DL_ESTABLISH_REQUEST 5
#define Q921_DL_RELEASE_REQUEST   8
#define Q921_DL_DATA_REQUEST      1


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

  // give L1 a chance to settle down
  sleep_seconds(1);
  
  assert(result == 0);
}

static void send_dl_establish_req(int data_socket)
{
  GTH_lapd_tx_su su;
  int result;

  // To activate the link, send DL_EST_REQ 
  su.length = htons(6);
  su.tag = 0;
  su.flags = 0x21;
  su.opcode = Q921_DL_ESTABLISH_REQUEST;
  su.reserved = 0;

  result = send(data_socket, (void*)&su, ntohs(su.length) + sizeof(su.length), 0);
  assert(result == 8);
}

static void send_dl_data_req(int data_socket)
{
  GTH_lapd_tx_su su;
  int result;

  su.length = htons(6 + 11);
  su.tag = 0;
  su.flags = 0x21;
  su.opcode = Q921_DL_DATA_REQUEST;
  su.reserved = 0;
  strcpy(su.payload, "hello world");

  result = send(data_socket, (void*)&su, ntohs(su.length) + sizeof(su.length), 0);
  assert(result == ntohs(su.length) + sizeof(su.length));
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
void read_exact(int fd, char *buf, size_t count) {
  size_t this_time;

  while (count > 0) {
    this_time = recv(fd, buf, count, 0);
    if (this_time <= 0) 
      die("LAPD data socket from GTH unexpectedly closed\n");

    count -= this_time;
    buf += this_time;
  }
}

// This function is the hook for Q.931. For now, it prints out 
// data and tries to establish a data link.
static void dump_incoming_lapd(int data_socket)
{
  short length;
  GTH_lapd_rx_su su;

  send_dl_establish_req(data_socket);

  for (;;)
    {
      length = 0;
      read_exact(data_socket, (char *)&length, 2);
      length = ntohs(length);
      read_exact(data_socket, (char *)&su, length);
      
      printf("LAPD data, tag=%d flags=%x opcode=%d length=%d",
	     su.tag, su.flags, su.opcode, length);
      if (length == 7)
	printf(" data=%c\n", su.payload[0]);
      else 
	printf("\n");

      if (su.opcode == 7)
	{
	  printf("sending dl-establish-req\n");
	  send_dl_establish_req(data_socket);
	}

      if (su.opcode == 6)
	{
	  printf("sending dl-data-req\n");
	  send_dl_data_req(data_socket);
	}
    }
}

// Entry point 
int main(int argc, char** argv) 
{
  GTH_api api;
  int data_socket;
  int result;

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

  fprintf(stderr, "lapd started, press ^C to abort\n");
  dump_incoming_lapd(data_socket);

  return 0; // not reached
}

// eof
