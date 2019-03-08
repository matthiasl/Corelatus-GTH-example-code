//----------------------------------------------------------------------
// An example program. Shows how to log broadcast events (alarms, alerts,
// L1 status changes, etc) from a GTH.
//
// Typical use is to leave this program running in a separate window while
// experimenting.
//
//
// Author: Thomas Lange (thomas@corelatus.se)
//
// Copyright (c) 2019, Corelatus AB Stockholm
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

static void
usage(void)
{
  fprintf(stderr,
	  "print_broadcast_events git_head: %s build_hostname: %s\n\n"

	  "print_broadcast_events <GTH-IP>\n\n"
	  "Show all broadcast events from a GTH.\n\n"

	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "Example:\n"
	  "./print_broadcast_events 172.16.1.10\n",
	  git_head, build_hostname
	  );

  exit(-1);
}

// Entry point
int
main(int argc, char **argv)
{
  GTH_api api;

  win32_specific_startup();

  if(argc != 2) usage();

  if(gth_connect(&api, argv[1], 0)){
    die("Unable to connect to the GTH. Giving up.");
  }

  while (0 == gth_process_event(&api))
    /* empty body */;

  die("event processing ended");

  return 0;
}

// eof
