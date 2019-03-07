//----------------------------------------------------------------------
// Minimal program to enable SDH/SONET and E1/T1 links
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
	  "enable git_head: %s build_hostname: %s\n\n"

	  "enable [-v] <GTH-IP> <resource> [<attribute> <value>]\n\n"
	  "Enable an SDH/SONET or E1/T1 interface.\n\n"
	  "Multiple <attribute> <value> pairs may be given.\n\n"

	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<resource> is an SDH/SONET or E1/T1 link on the GTH (hint: 'query inventory')\n"
	  "<attribute> is one of the resource's attributes\n"
	  "<value> is one of the resource's attributes\n"

	  "Examples:\n"
	  "./enable 172.16.1.10 sdh1\n"
	  "./enable 172.16.1.10 sdh1 AU 4  TU 12\n"
	  "./enable 172.16.1.10 sdh1 SONET true  OC 3  VT 2\n"
	  "./enable 172.16.1.10 pcm1A\n"
	  "./enable 172.16.1.10 pcm2A  framing multiframe\n"
	  "./enable 172.16.1.10 pcm2A  framing multiframe  monitoring true\n",
	  git_head, build_hostname
	  );

  exit(-1);
}

#define MAX_ATTRIBUTES 100

// Entry point
int main(int argc, char** argv)
{
  int result;
  GTH_api api;
  int verbose = 0;
  GTH_attribute attrs[MAX_ATTRIBUTES];
  int n_attrs = 0;

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'v': verbose = 1; break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc < 3) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }
  api.event_handler = &gth_silent_event_handler;

  // First attribute is in argv[3], first value in argv[4]
  if (argc >= MAX_ATTRIBUTES) {
    die("Too many name/value pairs. Abort.");
  }

  for (n_attrs = 0; n_attrs < (argc - 3) / 2; n_attrs++) {
    attrs[n_attrs].key   = argv[n_attrs * 2 + 3];
    attrs[n_attrs].value = argv[n_attrs * 2 + 4];
  }
  result = gth_enable(&api, argv[2], attrs, n_attrs);
  if (result != 0) {
    fprintf(stderr, "failed to enable %s\n", argv[2]);
    exit(1);
  }

  gth_bye(&api);

  return 0;
}
