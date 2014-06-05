//----------------------------------------------------------------------
// Example code to show how to put a start script on a Corelatus GTH.
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
//
//----------------------------------------------------------------------

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "gth_win32_compat.h"
#include "gth_apilib.h"
#include "gth_client_xml_parse.h"

static void usage()
{
  fprintf(stderr,
	  "install_start_script git_head: %s build_hostname: %s\n\n"

	  "install_start_script <GTH-IP> <filename>\n\n"
	  "Installs a start script on a GTH\n\n"

	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<filename> is a file containing an XML start script\n\n"

	  "Typical invocation: \n"
	  "    ./install_start_script 172.16.1.10 /tmp/start.xml\n\n",
	  git_head, build_hostname);
  exit(-1);
}

//------------------------------
static void install_start_script(const char *hostname,
				 const char *filename,
				 int verbose)
{
  GTH_api api;
  int result;
  FILE *script;
  int script_length;
  char *script_data;

  result = gth_connect(&api, hostname, verbose);
  if (result != 0) {
    die("Unable to connect to the GTH. Giving up.");
  }

  fprintf(stderr, "installing start script %s\n", filename);

  script = fopen(filename, "rb");
  if (!script) {
    die("unable to open start script file");
  }

  fseek(script, 0, SEEK_END);
  script_length = ftell(script);
  rewind(script);

  script_data = checked_malloc(script_length);
  assert(script_data);
  result = fread(script_data, script_length, 1, script);

  result = gth_install(&api, "start_script", "binary/file",
		       script_data, script_length);
  free(script_data);

  if (result != 0) {
    die("install failed");
  }
}

//------------------------------
static void print_e1t1_names(const char *hostname) {
  GTH_api api;
  int result = gth_connect(&api, hostname, 0);
  GTH_attribute *attributes;
  int n_attributes;
  int x;

  result = gth_query_resource(&api, "inventory", &attributes, &n_attributes);
  if (result != 0) {
    die("Unable to query the GTH inventory. Giving up.");
  }

  printf("Inventory:\n");
  for (x = 0; x < n_attributes; x++) {
    printf("  %s\n", attributes[x].key);
  }

  gth_free_attributes(attributes, n_attributes);

  gth_bye(&api);
}

//------------------------------
int main(int argc, char **argv)
{
  const char* hostname;
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

  hostname = argv[1];

  gth_switch_to(hostname, "failsafe", 1);

  install_start_script(hostname, argv[2], verbose);

  gth_switch_to(hostname, "system", 1);

  // Print the E1/T1 names. Two reasons for doing that. Firstly, so we
  // can see that the GTH isn't running failsafe. Secondly, because
  // the most common use of a start script is to change the E1/T1 naming.
  print_e1t1_names(hostname);

  return 0;
}
