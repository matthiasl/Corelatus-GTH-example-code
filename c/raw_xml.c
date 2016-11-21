//----------------------------------------------------------------------
// Minimal program to send raw XML commands to a GTH.
// Intended for debugging.
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2016, Corelatus AB Stockholm
//
// All rights reserved.
//
// BSD Licence:
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
	  "raw_xml git_head: %s build_hostname: %s\n\n"

	  "raw_xml <GTH-IP> <command>\n\n"
	  "Send a raw XML command to a GTH and print the result.\n\n"

	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<command> is an XML command, as per the GTH API manual.\n"

	  "Example:\n"
	  "./raw_xml 172.16.1.10 \"<query verbose='true'><job id='ldmo137'/></query>\n",
	  git_head, build_hostname);

  exit(-1);
}


// Entry point
int main(int argc, char** argv)
{
  int result;
  GTH_api api;

  if (argc != 3) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], 1 /* verbose */);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  gth_raw_xml(&api, argv[2]);

  gth_bye(&api);

  return 0;
}
