//----------------------------------------------------------------------
// Install a software image on a GTH module.
//
// Software images are available from http://www.corelatus.com/gth/releases/
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
	  "install_release [-f] [-v] <GTH-IP> <filename>\n\n"

	  "installs a software image on a GTH\n\n"

	  "-f: Install to failsafe image\n"
	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<filename> is the firmware image from www.corelatus.com\n\n"

	  "Example: \n"
	  "    ./install_release 172.16.1.10 gth2_system_33c.gth\n\n");
  exit(-1);
}


//------------------------------
static void install_release(const char *hostname,
			    const char *filename,
			    int verbose,
			    int failsafe)
{
  GTH_api api;
  int result;
  FILE *image;
  int image_length;
  char *image_data;
  char *image_name;

  if(failsafe){
    image_name = "failsafe_image";
  }
  else{
    image_name = "system_image";
  }

  result = gth_connect(&api, hostname, verbose);

  fopen_s(&image, filename, "rb");

  fprintf(stderr, "installing software image %s\n", filename);

  if (!image) die("unable to open software image file");

  fseek(image, 0, SEEK_END);
  image_length = ftell(image);
  rewind(image);

  image_data = malloc(image_length);
  assert(image_data);
  result = fread(image_data, image_length, 1, image);
  assert(result == 1);

  fclose(image);

  result = gth_set_single(&api, image_name, "locked", "false");
  assert(result == 0);

  result = gth_install(&api, image_name, "binary/filesystem",
		       image_data, image_length);
  free(image_data);

  if (result != 0) die("install failed");
}

static void show_releases(const char *hostname, const int verbose)
{
  GTH_api api;
  int result;
  char attribute[1000];

  result = gth_connect(&api, hostname, verbose);

  if (result != 0) {
    die("unable to connect to GTH");
  }

  result = gth_query_resource_attribute(&api, "system_image", "version",
					attribute, sizeof(attribute));
  assert(result == 0);

  printf("  Current system image version: %s\n", attribute);

  result = gth_query_resource_attribute(&api, "failsafe_image", "version",
					attribute, sizeof(attribute));
  assert(result == 0);

  printf("Current failsafe image version: %s\n", attribute);


  gth_bye(&api);
}

//------------------------------
int main(int argc, char **argv)
{
  const char* hostname;
  const char* filename;
  char* target;
  char* other;
  int verbose = 0;
  int failsafe = 0;
  int chr;

  while (argc > 1 && argv[1][0] == '-') {
    chr = 1;
    while(argv[1][chr] != 0){
      switch (argv[1][chr]) {
      case 'v': verbose = 1; break;

      case 'f': failsafe = 1; break;

      default: usage();
      }
      chr++;
    }
    argc--;
    argv++;
  }

  if (argc != 3) {
    usage();
  }

  win32_specific_startup();

  hostname = argv[1];

  filename = argv[2];

  if (strstr(filename, "_failsafe_") && !failsafe){
    die("Refusing to install this unless -f is given");
  }

  if (strstr(filename, "_system_") && failsafe){
    die("Refusing to install this when -f is given");
  }

  if(failsafe){
    target = "failsafe";
    other = "system";
  }
  else{
    target = "system";
    other = "failsafe";
  }

  show_releases(hostname, verbose);

  gth_switch_to(hostname, other, 1);

  install_release(hostname, filename, verbose, failsafe);

  gth_switch_to(hostname, target, 1);

  show_releases(hostname, verbose);

  return 0;
}
