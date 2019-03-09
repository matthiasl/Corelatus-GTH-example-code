//----------------------------------------------------------------------
// Minimal program to record a timeslot on a GTH to a file.
//
// The recording is a byte-by-byte recording of the timeslot's contents.
// Normally no header is added. But: if filename ends in .wav, a WAV header
// is prepended.
//
// Limitations:
//   Doesn't check of L1 status
//   Doesn't log L1 errors and status changes
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
//
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <assert.h>
#include <string.h>

#ifdef WIN32
#include <winsock2.h>
#else
#include <sys/socket.h>
#include <unistd.h>
#endif

#include "gth_win32_compat.h"
#include "gth_apilib.h"

static void usage(void)
{
  fprintf(stderr,
	  "record git_head: %s build_hostname: %s\n\n"

	  "record [-vlT] <GTH-IP> <span> <timeslot> <filename>\n\n"
	  "Save bit-exact data from a timeslot to a file\n\n"
	  "-v: print the API commands and responses (verbose)\n"
          "-l: do not set up L1; assume it's already set up\n"
	  "-T: use T1 (and mulaw) instead of the default E1 L1 setup\n"
	  "<GTH-IP> is the GTH's IP address or hostname\n"
	  "<span> is the E1/T1 interface, e.g. '1A'\n"
	  "<timeslot> is the timeslot, 1--31\n"
	  "<filename> is the filename to save to. '-' means standard output\n\n",
	  git_head, build_hostname);

  fprintf(stderr, "Typical use:\n");
  fprintf(stderr, "./record    172.16.1.10 1A 1 my_capture.wav\n\n");
  fprintf(stderr, "./record -T 172.16.1.10 1A 1 my_capture.wav\n\n");

  exit(-1);
}

typedef unsigned int   u32;
typedef unsigned short u16;

// The WAV header format, as described in
//
// http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/WAVE.html
#pragma pack(1)
struct WAV_header {
  char ckID[4];
  u32 cksize;         // always 20 + the number of samples
  char WAVEID[4];

  //----
  char format_id[4];
  u32 format_size;    // always 16 for this minimal header

  u16 wFormatTag;
  u16 nChannels;
  u32 nSamplesPerSec;
  u32 nAvgSamplesPerSec;
  u16 nBlockAlign;
  u16 wBitsPerSample;

  //----
  char DATAID[4];
  u32 data_size;
} PACK_SUFFIX;

#if !(__BYTE_ORDER == __LITTLE_ENDIAN)
#error "the .wav header will won't work"
// If you want, you can disable this compile-time error. The recording
// tool will still work, but the .wav header will be corrupt.
#endif

static void possibly_prepend_wav_header(const char *filename,
					FILE *file,
					const int mulaw)
{
  int len = strlen(filename);

  if (len < 4)
    return;

  if (strcmp(filename + len - 4, ".wav") == 0)
    {
      int result;
      struct WAV_header header;
      char riff[] = "RIFF";
      char wave[] = "WAVE";
      char fmt[] = "fmt ";
      char data[] = "data";

      // If the header is longer than expected, the compiler must have
      // padded the structure. We can't have that.
      assert(sizeof(header) == 44);

      memcpy(header.ckID, riff, 4);
      header.cksize = 36 + 8000; // fill in a bogus length
      memcpy(header.WAVEID, wave, 4);
      memcpy(header.format_id, fmt, 4);
      header.format_size = 16;
      header.wFormatTag = (mulaw)?7:6;  // 6 is a magic number for 8kHz A-law
      header.nChannels = 1;
      header.nSamplesPerSec = 8000;
      header.nAvgSamplesPerSec = 8000;
      header.nBlockAlign = 1;
      header.wBitsPerSample = 8;

      memcpy(header.DATAID, data, 4);
      header.data_size = 80000;

      result = fwrite(&header, sizeof(header), 1, file);

      assert(result == 1);
    }
}

static void record_a_file(GTH_api *api,
			  const char *span,
			  const int timeslot,
			  const char *filename,
			  const int mulaw)
{
  int data_socket;
  char buffer[2000];
  ssize_t octet_count;
  int octet_sum = 0;
  char job_id[MAX_JOB_ID];
  int result;
  FILE* file = 0;

  if ( (timeslot < 1) || (timeslot > 31) ) {
    fprintf(stderr, "valid timeslots are 1--31, not %d. Aborting\n", timeslot);
    exit(-1);
  }

  if ( strcmp(filename, "-") == 0 ) {
    file = stdout;
  } else {
    fopen_s(&file, filename, "wb");
    if (file == 0) {
      fprintf(stderr, "unable to open %s, aborting\n", filename);
      exit(-1);
    }
  }

  possibly_prepend_wav_header(filename, file, mulaw);

  data_socket = gth_new_recorder(api, span, timeslot, job_id);
  if (data_socket < 0) {
    die("unable to start a <recorder> on the GTH. -v gives more information");
  }

  fprintf(stderr, "started recording. Press ^C to end.\n");

  while ( (octet_count = recv(data_socket, buffer, sizeof buffer, 0)) ) {
    result = fwrite(buffer, octet_count, 1, file);
    if (result != 1) {
      die("Writing to the output file failed. Giving up.");
    }
    fprintf(stderr, "%d ", octet_sum);
    octet_sum += octet_count;
  }
  closesocket(data_socket);
  fclose(file);

  fprintf(stderr, "wrote %d octets to %s\n", octet_sum, filename);
}

// Set the mode for the given E1/T1, using default parameters.
//
// This works for many setups, but not all. The possible problems are:
//
//   - GTH 2.x hardware has restrictions on mixing E1 and T1 modes.
//     This might hit someone trying out options. They'll get a 'conflict'
//     error. If in doubt, disable all the E1/T1 before changing modes,
//     for instance by cycling power.
//
//   - The T1 default settings might not be right for a particular site.
//     Check the L1 status page on the in-built webserver (port 8888)
//     to see.
//
static void setup_layer_1(GTH_api *api, const char *pcm_name, const int mulaw) {
  int result;
  const char e1[] = "E1";
  const char t1[] = "T1";

  result = gth_set_single(api, pcm_name, "mode", (mulaw)?t1:e1);

  if (result != 0) {
    fprintf(stderr, "Layer 1 setup failed, useless recording likely.\n");
  }
}


// Entry point
int main(int argc, char** argv)
{
  int result;
  GTH_api api;
  char pcm_name[20];
  int t1_mulaw_mode = 0;
  int verbose = 0;
  int setup_l1 = 1;

  while (argc > 1 && argv[1][0] == '-') {
    switch (argv[1][1]) {
    case 'v': verbose = 1; break;
    case 'l': setup_l1 = 0; break;
    case 'T': t1_mulaw_mode = 1; break;

    default: usage();
    }
    argc--;
    argv++;
  }

  if (argc != 5) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1], verbose);
  if (result != 0) {
    die("unable to connect to the GTH");
  }

  assert(sizeof(pcm_name) > (strlen("pcm") + strlen(argv[2])));
  strncpy_s(pcm_name, sizeof pcm_name, "pcm", sizeof pcm_name - 1);
  strncat(pcm_name, argv[2], sizeof pcm_name - strlen(pcm_name) - 1);

  if (setup_l1) {
    setup_layer_1(&api, pcm_name, t1_mulaw_mode);
  }

  record_a_file(&api, argv[2], checked_atoi(argv[3]), argv[4], t1_mulaw_mode);

  gth_bye(&api);

  fprintf(stderr, "all done\n");

  return 0;
}
