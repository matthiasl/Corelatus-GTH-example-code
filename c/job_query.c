//----------------------------------------------------------------------
// Query job attributes on a GTH
//
// Doesn't attempt any error handling.
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2016, Corelatus AB Stockholm
//
// All rights reserved. Licence: BSD.
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
#include <string.h>

#ifdef WIN32
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <netinet/in.h>
#endif // WIN32

#include "gth_win32_compat.h"
#include "gth_apilib.h"

static void usage()
{
  fprintf(stderr,
	  "job_query git_head: %s build_hostname: %s\n\n"

	  "job_query [-v] <GTH-IP> <id>\n\n"
	  "Query job attributes on a GTH.\n\n"

	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<id> is the ID of a job (hint: the 'schedule' resource is a list of jobs)\n"

	  "Examples:\n"
	  "./job_query 172.16.1.10 m2mo3\n",
	  git_head, build_hostname);

  exit(-1);
}

static void query_job(GTH_api *api, const char *id)
{
  int n_attributes;
  GTH_attribute *attrs;
  char owner[MAX_JOB_ID + 1];
  int result;
  int x;

  result = gth_query_job(api, id, owner, &attrs, &n_attributes);
  if (result != 0)
    {
      fprintf(stderr, "unable to query %s\n", id);
      gth_bye(api);
      exit(-1);
    }

  if (n_attributes == 0)
    printf("%s has no attributes\n", id);

  for (x = 0; x < n_attributes; x++)
    printf("%s=%s\n", attrs[x].key, attrs[x].value);
}


#define MAX_ATTRIBUTES 100

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

  if (argc != 3) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }
  api.event_handler = &gth_silent_event_handler;

  query_job(&api, argv[2]);

  gth_bye(&api);

  return 0;
}
