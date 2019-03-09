//----------------------------------------------------------------------
// Minimal program to connect one or more timeslots
//
// Doesn't attempt any error handling.
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
//
//----------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>

#include "gth_win32_compat.h"
#include "gth_apilib.h"

static void usage(void)
{
  fprintf(stderr,
	  "connect_timeslots git_head: %s build_hostname: %s\n\n"

	  "connect_timeslots [-v] <GTH-IP> <src span> <src ts> <dst span> <dest ts> ...\n"
	  "Connect the given timeslot a GTH.\n\n"
          "Multiple sources and sinks can be given. You may want\n"
          "to use 'query_set' to set up L1 before starting.\n\n"

	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<src span> is the E1/T1 interface to read from, e.g. '1A'\n"
	  "<src ts> is the timeslot to read from, 1--31\n"
	  "<dst span> is the E1/T1 interface to write to, e.g. '1A'\n"
	  "<dst ts> is the timeslot to write to, 1--31\n\n"

	  "Examples:\n"
	  "./connect_timeslots 172.16.1.10 1A 16 2A 16\n"
	  "./connect_timeslots 172.16.1.10 1A 1 2A 13  1A 2 3A 1  2A 13 1A 1\n"
	  "./connect_timeslots 172.16.1.10 1A 16 2A 16  2A 16 1A 16\n",
	  git_head, build_hostname);
  exit(-1);
}

// Different hardware versions have different maximum connection numbers.
// 100 works on all of them and is plenty for experiments.
#define MAX_CONNECTIONS 100

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

  if (argc < 6 || (argc - 2) % 4 != 0) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0)
    die("Unable to connect to the given GTH. Giving up.");

  argv += 2;
  argc -= 2;

  while (argc >= 4)
    {
      char job_id[MAX_JOB_ID];

      result = gth_new_connection(&api,
				  argv[0], checked_atoi(argv[1]),
				  argv[2], checked_atoi(argv[3]),
				  job_id);

      fprintf(stderr, "connecting %s:%s -> %s:%s. ",
	      argv[0], argv[1], argv[2], argv[3]);

      if (result == 0)
	fprintf(stderr, "Ok.\n");
      else
	fprintf(stderr, "Unable to set up connection. Skipping.\n");

      argv += 4;
      argc -= 4;
    }

  fprintf(stderr, "press ^C to shut down all timeslot connections\n");

  while (1)
    {
      gth_nop(&api);
      sleep_seconds(1);
    }

  return 0;
}
