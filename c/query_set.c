//----------------------------------------------------------------------
// Minimal program to query or set resource parameters on a GTH
//
// Doesn't attempt any error handling.
// 
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2010, Corelatus AB Stockholm
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

#include <assert.h>

#include "gth_win32_compat.h"
#include "gth_apilib.h"

static void usage() 
{
  fprintf(stderr, "query_set <GTH-IP> <resource> [<attribute> [<value>]]\n\n"
	  "Query or set resource parameters on a GTH.\n\n"
	  "If no <value> is given, just query the GTH.\n"
	  "If a <value> _is_ given, set the attribute to the value.\n"
	  "Multiple <attribute> <value> pairs may be given.\n\n"

	  "Examples:\n"
	  "./query_set 172.16.1.10 pcm1A\n"
	  "./query_set 172.16.1.10 code_violations\n"
	  "./query_set 172.16.1.10 pcm1A status enabled\n"
	  "./query_set 172.16.1.10 pcm1A status enabled framing multiframe\n");
  
  exit(-1);
}

static void query_resource(GTH_api *api, const char *name, const char *key)
{
  int n_attributes;
  GTH_attribute *attrs;
  int result;
  int x;

  result = gth_query_resource(api, name, &attrs, &n_attributes);
  if (result != 0)
    {
      fprintf(stderr, "unable to query %s\n", name);
      gth_bye(api);
      exit(-1);
    }

  if (!strcmp(name, "inventory")) 
    {
      for (x = 0; x < n_attributes; x++)
	{
	      printf("%s\n", attrs[x].key);
	}
    }
  else
    {
      for (x = 0; x < n_attributes; x++)
	{
	  if (key == 0 || !strcmp(attrs[x].key, key)) 
	    {
	      printf("%s=%s\n", attrs[x].key, attrs[x].value);
	    }
	}
    }
}

#define MAX_ATTRIBUTES 100

// Entry point 
int main(int argc, char** argv) 
{
  int result;
  GTH_api api;

  if (argc < 2) {
    usage();
  }

  win32_specific_startup();

  result = gth_connect(&api, argv[1]);
  assert(result == 0);

  if (argc == 2)  // no resource given, do an inventory query
    {
      fprintf(stderr, "no resource given, printing inventory\n");
      query_resource(&api, "inventory", 0);
    }

  else if (argc == 3)  // plain query
    {
      query_resource(&api, argv[2], 0);
    }

  else if (argc == 4)  // single attribute query
    {
      query_resource(&api, argv[2], argv[3]);
    }
  
  else
    {
      GTH_attribute attrs[MAX_ATTRIBUTES];
      int n_attrs = 0;

      // First attribute is in argv[3], first value in argv[4]

      assert(argc >= 5 && argc < MAX_ATTRIBUTES);  // conservative
      for (n_attrs = 0; n_attrs < (argc - 3) / 2; n_attrs++)
	{
	  attrs[n_attrs].key   = argv[n_attrs * 2 + 3];
	  attrs[n_attrs].value = argv[n_attrs * 2 + 4];
	}
      result = gth_set(&api, argv[2], attrs, n_attrs);
      if (result != 0)
	{
	  fprintf(stderr, "failed to set attributes on %s\n", argv[2]);
	}
    }

  gth_bye(&api);

  return 0;
}

