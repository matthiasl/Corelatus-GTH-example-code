//----------------------------------------------------------------------
// Minimal program to map an E1/T1 link carried on SDH/SONET.
// Once an E1/T1 is mapped, it has a name, e.g. pcm27, which can
// then be used in further commands, for instance 'enable'.
//
// Doesn't attempt any error handling.
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
	  "map git_head: %s build_hostname: %s\n\n"

	  "map [-v] <GTH-IP> <sdh-resource>\n\n"
	  "Map an E1/T1 link carried on SDH/SONET. Print the name of the\n"
          "E1/T1 resource created by the mapping.\n\n"

	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<resource> is an SDH/SONET LOP resource\n"

	  "Example:\n"
	  "./map 172.16.1.10 sdh1:hop1_1:lop1_5_3\n",
	  git_head, build_hostname);

  exit(-1);
}

#define MAX_RESOURCE_NAME 100

// Entry point
int main(int argc, char** argv)
{
  int result;
  GTH_api api;
  int verbose = 0;
  char name[MAX_RESOURCE_NAME];

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

  result = gth_map(&api, argv[2], name, MAX_RESOURCE_NAME);

  if (result == 0) {
    printf("%s\n", name);
  }
  else {
    fprintf(stderr, "unable to map %s. Re-run with -v for more info.\n", name);
  }

  gth_bye(&api);

  return 0;
}

