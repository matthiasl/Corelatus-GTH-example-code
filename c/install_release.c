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
#include <unistd.h>

#include "gth_apilib.h"
#include "gth_client_xml_parse.h"

static void usage() 
{
  fprintf(stderr, "install_release: installs a software image on a GTH\n\n");
  fprintf(stderr, "Typical invocation: \n");
  fprintf(stderr, "    ./install_release 172.16.1.10 gth2_system_33c.gth\n\n");
  exit(-1);
}


//------------------------------
static void install_release(const char *hostname, const char *filename)
{
  GTH_api api;
  int result = gth_connect(&api, hostname);
  FILE *image = fopen(filename, "rb");
  int image_length;
  char *image_data;

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

  result = gth_set_single(&api, "system_image", "locked", "false");
  assert(result == 0);

  result = gth_install(&api, "system_image", "binary/filesystem", 
		       image_data, image_length);
  assert(result == 0);

  free(image_data);

  if (result != 0) die("install failed");
}

static void show_releases(const char *hostname)
{
  GTH_api api;
  int result = gth_connect(&api, hostname);
  char attribute[1000];

  if (result != 0) 
    die("unable to connect to GTH");

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

  if (argc != 3) {
    usage();
  }

  win32_specific_startup();

  hostname = argv[1];

  show_releases(hostname);

  gth_switch_to(hostname, "failsafe", 1);

  install_release(hostname, argv[2]);

  gth_switch_to(hostname, "system", 1);

  show_releases(hostname);

  return 0;
}
