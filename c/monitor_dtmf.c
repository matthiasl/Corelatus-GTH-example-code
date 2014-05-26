//----------------------------------------------------------------------
// Minimal program to demonstrate how DTMF detection is set up on
// a Corelatus GTH.
//
// Also demonstrates how to handle asynchronous events (in this case, DTMF
// tones) from the GTH by using select(). Use select() instead of
// poll() so that this example works on Win32 as well.
//
// Doesn't attempt any error handling.
// Doesn't keep track of L1 status
// Doesn't log L1 errors and status changes
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2011, Corelatus AB Stockholm
//
// Licence: BSD
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
#include <sys/select.h>
#endif // WIN32

#include <assert.h>
#include <string.h>

#include "gth_win32_compat.h"
#include "gth_apilib.h"

static void usage()
{
  fprintf(stderr,
	  "monitor_dtmf git_head: %s build_hostname: %s\n\n"

	  "monitor_dtmf [-v] <GTH-IP> <span> <timeslot>.\n\n"
	  "Set up DTMF monitoring on a GTH and print all received tones\n\n"

	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n\n"
	  "<span> is the E1/T1 interface, e.g. '1A'\n"
	  "<timeslot> is the timeslot, 1--31\n\n"

	  "Typical use:\n"
	  "monitor_dtmf 172.16.1.10 1A 23\n",
	  git_head, build_hostname);

  exit(-1);
}

void tone_handler(const char *name, int length)
{
  printf("detected DTMF digit %s (duration: %d ms)\n", name, length);
}

// Entry point
int main(int argc, char** argv)
{
  int result;
  GTH_api api;
  char job_id[MAX_JOB_ID];
  char pcm_name[20];
  fd_set readfds;
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
    die("Unable to connect to the GTH. Giving up.");
  }

  // Enable L1 with default parameters. If you want to use monitor
  // mode or multiframe or ... see save_to_pcap.c
  result = snprintf(pcm_name, sizeof(pcm_name), "pcm%s", argv[2]),
  assert(result < sizeof(pcm_name));
  result = gth_set_single(&api, pcm_name, "status", "enabled");
  if (result != 0) {
    die("unable to enable L1. (use -v to see more information)");
  }

  gth_new_tone_detector(&api, argv[2], atoi(argv[3]), job_id, &tone_handler);

  FD_ZERO(&readfds);
  FD_SET(api.fd, &readfds);

  while (1) {
    result = select(api.fd + 1, &readfds, 0, 0, 0);
    assert(result == 1);
    gth_nop(&api);
  }

  return 0;
}
