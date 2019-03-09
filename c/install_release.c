//----------------------------------------------------------------------
// Install a software image on a GTH module.
//
// Software images are available from https://www.corelatus.com/gth/releases/
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

// Microsoft VC13 only provides basename() in C++, not in C.
#ifdef _MSC_VER
#define MAX_FILENAME 80
char *basename(const char *path)
{
  errno_t result;
  static char filename[MAX_FILENAME];

  result = _splitpath_s(path,
                        0, 0, // drive
                        0, 0, // directory
                        filename, MAX_FILENAME,
                        0, 0);// extension
  if (result != 0)
    filename[0] = 0;

  return filename;
}
#else
#include <libgen.h>
#endif

#include "gth_win32_compat.h"
#include "gth_apilib.h"
#include "gth_client_xml_parse.h"

struct Version {
  int major;
  char minor;
};

static struct Version VERSION_ZERO = {0, ' '};

enum Hardware {
  END_OF_TABLE,
  GTH_1_x,
  GTH_2_0,
  GTH_2_1,
  GTH_3_0,
  STH_3_0,
  HW_UNKNOWN
};

struct Compatibility {
  enum Hardware hardware;
  int desired_is_failsafe;
  struct Version desired;
  struct Version minimum_required;
};

// All the version dependencies we know about.
//
// The 'desired' column must be in ascending order within the same
// release type.
struct Compatibility compatibility[] =
  // Hardware  Is_failsafe    desired         minimum_required
  {{ STH_3_0,  0,             {40, 'a'},      {13, 'a'} },
   { STH_3_0,  1,             {15, 'a'},      {38, 'a'} },

   { GTH_3_0,  0,             {40, 'a'},      {13, 'a'} },
   { GTH_3_0,  1,             {15, 'a'},      {38, 'a'} },

   { GTH_2_1,  0,             {36, 'b'},      { 9, 'a'} },
   { GTH_2_1,  1,             { 9, 'a'},      {33, 'a'} },

   { GTH_2_0,  0,             {36, 'b'},      { 7, 'b'} },
   { GTH_2_0,  1,             { 7, 'b'},      {31, 'a'} },
   { GTH_2_0,  1,             { 9, 'a'},      {33, 'a'} },

   { GTH_1_x,  0,             {31, 'a'},      { 5, '.'} },

   { END_OF_TABLE, 0,         {0, 0},         {0, 0}}};


static void usage(void)
{
  fprintf(stderr,
	  "install_release git_head: %s build_hostname: %s\n\n"

	  "install_release [-f] [-n] [-v] <GTH-IP> <filename>\n\n"

	  "installs a software image on a GTH\n\n"

	  "-f: Install to failsafe image\n"
	  "-n: No sanity checks (do not check for impossible upgrades)\n"
	  "-v: print the API commands and responses (verbose)\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<filename> is the firmware image from www.corelatus.com\n\n"

	  "Example: \n"
	  "    ./install_release 172.16.1.10 gth2_system_33c.gth\n\n",
	  git_head, build_hostname);
  die("");
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

  fprintf(stderr, "installing software image %s\n", filename);

  fopen_s(&image, filename, "rb");

  if (image)
    {
      fseek(image, 0, SEEK_END);
      image_length = ftell(image);
      rewind(image);

      image_data = checked_malloc(image_length);
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
  else
    {
      die("unable to open software image file");
    }
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

static char to_lower(char c)
{
  return c | 32;
}

static int case_insensitive_strncmp(const char* s1, const char* s2, int n)
{
  int x;

  for (x = 0; x < n; x++)
    {
      char c1 = to_lower(s1[x]);
      char c2 = to_lower(s2[x]);
      if ( c1 > c2 ) return 1;
      if ( c1 < c2 ) return -1;
      if (c1 == 0 || c2 == 0) return 0;
    }

  return 0;
}

// s: version string, e.g. gth2_failsafe_15b
static void extract_version(const char* s, struct Version *v)
{
  *v = VERSION_ZERO;

  s = strchr(s, '_');

  if (!s)
    return;

  s++;
  s = strchr(s, '_');

  if (!s)
    return;

  s++;
  sscanf(s, "%d%c", &(v->major), &(v->minor));
  v->minor = to_lower(v->minor);
}

enum Hardware arch_to_hw(const char* arch)
{
  if (strcmp(arch, "sth3.0") == 0) return STH_3_0;
  if (strcmp(arch, "gth3.0") == 0) return GTH_3_0;
  if (strcmp(arch, "gth2.1") == 0) return GTH_2_1;
  if (strcmp(arch, "gth2.0") == 0) return GTH_2_0;

  // 32c and earlier report "gth2". Assume worst case.
  if (strcmp(arch, "gth2") == 0)   return GTH_2_0;

  if (strncmp(arch, "gth1", 4) == 0) return GTH_1_x;

  // Probably using an old version of 'install_release.c' on newer hardware.
  fprintf(stderr, "Warning: unknown hardware type. Continuing anyway.\n");
  return HW_UNKNOWN;
}

static int version_at_least(struct Version a, struct Version b)
{
  if (a.major > b.major) return 1;
  if (a.major == b.major && a.minor >= b.minor) return 1;
  return 0;
}

static int are_versions_incompatible(const char *arch,
				     const int  failsafe,
				     const char *installer,
				     const char *filename)
{
  struct Version installer_v;
  struct Version desired;
  struct Version minimum_required;

  struct Compatibility *compat;
  enum Hardware hw = arch_to_hw(arch);

  extract_version(installer, &installer_v);
  extract_version(filename,  &desired);

  compat = compatibility;

  minimum_required = VERSION_ZERO;

  while (compat->hardware != END_OF_TABLE)
    {
      if (compat->hardware == hw
	  && compat->desired_is_failsafe == failsafe
	  && version_at_least(desired, compat->desired))
	    minimum_required = compat->minimum_required;
      compat++;
    }

  if (minimum_required.major == 0)
    die("Release file too old. Dependencies unknown. Consider '-n'");

  if (!version_at_least(installer_v, minimum_required))
    {
      fprintf(stderr,
	      "Bad: hardware is running %d%c which is older than the "
	      "known minimum %d%c for this install.\n",
	      installer_v.major, installer_v.minor,
	      minimum_required.major, minimum_required.minor);
      return 1;
    }

  fprintf(stderr,
	  "Good: card is running %d%c; which is at least the minimum %d%c.\n",
	  installer_v.major, installer_v.minor,
	  minimum_required.major, minimum_required.minor);

  return 0;
}

static void
check_for_problems(const char *hostname,
		   const int verbose,
		   const int failsafe,
		   const char *filename,
		   const char *other)
{
  GTH_api api;
  int result;
  char *reason;
  char attribute[1000];
  char arch[1000];
  char image_name[1000];

  //--------------------
  reason = "Refusing to install a failsafe image unless -f is given";
  if (strstr(filename, "_failsafe_") && !failsafe)
    die(reason);

  //--------------------
  reason = "Refusing to install a system image when -f is given";
  if (strstr(filename, "_system_") && failsafe)
    die(reason);

  result = gth_connect(&api, hostname, verbose);

  if (result != 0)
    die("unable to connect to GTH");

  //--------------------
  // The 'architecture' query fails on GTH 1.x ancient releases, e.g. fs3.
  // Never mind, nobody is still using them.
  reason = "Release filename is not reasonable for this hardware.";
  result = gth_query_resource_attribute(&api, "board", "architecture",
					arch, sizeof(arch));
  assert(result == 0);
  if (case_insensitive_strncmp(arch, filename, 4) != 0)
    {
      fprintf(stderr, "Architecture is %s, release filename is '%s'\n",
	      arch, filename);
      die(reason);
    }

  //--------------------
  reason = "Upgrade would require booting to an 'empty' firmware image";
  if (strcmp(other, "empty") == 0)
    die(reason);

  //--------------------
  reason = "Upgrade would fail because of version dependencies (consider -n)";
  image_name[0] = 0;
  strncat(image_name, other, sizeof(image_name) - 1);
  strncat(image_name, "_image", sizeof(image_name) - 1 - strlen(other));
  result = gth_query_resource_attribute(&api, image_name, "version",
					attribute, sizeof(attribute));
  assert(result == 0);

  if (are_versions_incompatible(arch, failsafe, attribute, filename))
    die(reason);

  gth_bye(&api);
}

//------------------------------
int main(int argc, char **argv)
{
  const char* hostname;
  char* filename;
  char* target;
  char* other;
  int verbose = 0;
  int failsafe = 0;
  int force = 0;
  int chr;

  while (argc > 1 && argv[1][0] == '-') {
    chr = 1;
    while(argv[1][chr] != 0){
      switch (argv[1][chr]) {
      case 'f': failsafe = 1; break;
      case 'n': force = 1;    break;
      case 'v': verbose = 1;  break;

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

  if(failsafe){
    target = "failsafe";
    other = "system";
  }
  else{
    target = "system";
    other = "failsafe";
  }

  show_releases(hostname, verbose);

  if (!force)
    check_for_problems(hostname, verbose, failsafe, basename(filename), other);

  gth_switch_to(hostname, other, 1);

  install_release(hostname, filename, verbose, failsafe);

  gth_switch_to(hostname, target, 1);

  show_releases(hostname, verbose);

  return 0;
}
