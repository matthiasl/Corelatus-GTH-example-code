//----------------------------------------------------------------------
// Minimal program to playback a file on a given timeslot on a Corelatus GTH
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
#include <errno.h>
#include <assert.h>
#include <string.h>

#ifdef WIN32
#include <winsock2.h>
#else
#include <unistd.h>
#include <sys/socket.h>
#endif

#include "gth_win32_compat.h"
#include "gth_apilib.h"

static void usage(void)
{
  fprintf(stderr,
	  "playback_file git_head: %s build_hostname: %s\n\n"

	  "playback_file [-v] <GTH-IP> <span> <timeslot> <filename>\n\n"
	  "Play the contents of a file on a timeslot.\n"
	  "\n-v: print the API commands and responses (verbose)"
	  "\n-l: do not set up L1; assume it's already set up"
	  "\n<GTH-IP> is the GTH's IP address or hostname"
	  "\n<span> is the name of a span, e.g. '1A'"
	  "\n<ts> is a timeslot number, from 1 to 31"
	  "\n<filename> is the file to read data from",
	  git_head, build_hostname);

  fprintf(stderr, "\n\nTypical use:\n");
  fprintf(stderr, "./playback_file 172.16.1.10 1A 1 audio/mfc_fwd_4\n");

  exit(-1);
}

#define MAX_COMMAND 200

static void play_a_file(GTH_api *api,
			const char *span,
			int timeslot,
			const char *filename)
{
  int data_socket;
  char buffer[1600];
  size_t octet_count;
  size_t octet_sum = 0;
  char job_id[MAX_COMMAND];
  ssize_t result;
  FILE* file = 0;

  if ( (timeslot < 1) || (timeslot > 31) ) {
    fprintf(stderr, "valid timeslots are 1--31, not %d. Aborting\n", timeslot);
    exit(-1);
  }

  fopen_s(&file, filename, "rb");
  if (file == 0) {
    fprintf(stderr, "unable to open %s, aborting\n", filename);
    exit(-1);
  }

  data_socket = gth_new_player(api, span, timeslot, job_id);
  if (data_socket < 0) {
    die("unable to start a player on the GTH. Use -v to see more details.");
  }

  while ( (octet_count = fread(buffer, 1, sizeof buffer, file)) ) {
    result = send(data_socket, buffer, octet_count, 0);
    if (result <= 0)
      die("unable to send data to the player TCP socket");
    fprintf(stderr, SIZE_T_FORMAT " ", octet_sum);
    octet_sum += octet_count;

    // send could return less than the requested amount, e.g. if there's
    // back-pressure from the GTH. We don't deal with that case in this code.
    assert((size_t)result == octet_count);
  }

  result = closesocket(data_socket);
  assert(result == 0);
  fclose(file);

  fprintf(stderr, "wrote "SIZE_T_FORMAT" octets to the <player>\n", octet_sum);

  gth_wait_for_message_ended(api, job_id);
}

// Entry point
int main(int argc, char **argv)
{
  int result;
  GTH_api api;
  char pcm_name[20];
  int verbose = 0;
  int setup_l1 = 1;

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {

    case 'v': verbose = 1; break;
    case 'l': setup_l1 = 0; break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc != 5) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  assert(sizeof(pcm_name) > (strlen("pcm") + strlen(argv[2])));
  strncpy_s(pcm_name, sizeof pcm_name, "pcm", sizeof pcm_name - 1);
  strncat(pcm_name, argv[2], sizeof pcm_name - 1);

  if (setup_l1)
    {
      gth_set_single(&api, pcm_name, "status", "enabled");
    }

  play_a_file(&api, argv[2], checked_atoi(argv[3]), argv[4]);

  gth_bye(&api);

  fprintf(stderr, "all done\n");

  return 0;
}
