#ifndef GTH_CLIENT_XML_PARSE_H
#define GTH_CLIENT_XML_PARSE_H

//----------------------------------------------------------------------
// Scanner and parser for the responses generated by a Corelatus GTH,
// which are documented in http://www.corelatus.com/gth/api/
//
// This scanner/parser is complete, i.e. it can parse all possible
// XML responses from a GTH.
//
// Typical use:
//
//   #include "gth_client_xml_parse.h"
//
//   GTH_resp *resp;
//   resp = gth_parse("<job id='m2mo33'/>");
//
//   ...manipulate the parse tree...
//
//   gth_free_resp(resp);
//
// If you are planning to build an application using this code, consider
// using gth_apilib.h instead, it provides a higher-level interface.
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
//----------------------------------------------------------------------

typedef enum {
  GTH_RESP_ALARM,
  GTH_RESP_ALERT,
  GTH_RESP_ATM_MESSAGE,
  GTH_RESP_ATTRIBUTE,
  GTH_RESP_BACKUP,

  GTH_RESP_CONTROLLER,
  GTH_RESP_EBS,
  GTH_RESP_ERROR,
  GTH_RESP_EVENT,
  GTH_RESP_FATALITY,

  GTH_RESP_FAULT,
  GTH_RESP_F_RELAY_MESSAGE,
  GTH_RESP_INFO,
  GTH_RESP_JOB,
  GTH_RESP_L1_MESSAGE,

  GTH_RESP_L2_ALARM,
  GTH_RESP_L2_SOCKET_ALERT,
  GTH_RESP_LAPD_MESSAGE,
  GTH_RESP_LAPD_MONITOR,
  GTH_RESP_LEVEL,
  GTH_RESP_MESSAGE_ENDED,

  GTH_RESP_MTP2_MESSAGE,
  GTH_RESP_MTP2_MONITOR,
  GTH_RESP_OK,
  GTH_RESP_RESOURCE,
  GTH_RESP_SDH_MESSAGE,
  GTH_RESP_SFP_MESSAGE,
  GTH_RESP_SLIP,
  GTH_RESP_STATE,
  GTH_RESP_SYNC_MESSAGE,

  GTH_RESP_TONE

} GTH_resp_type;

typedef struct {
  char *key;
  char *value;
} GTH_attribute;

struct GTH_resp_struct;

struct GTH_resp_struct {
  GTH_resp_type type;
  GTH_attribute *attributes;
  int n_attributes;
  int allocated_attributes;

  struct GTH_resp_struct *children;
  int n_children;
  int allocated_children;

  char *text;
};

typedef struct GTH_resp_struct GTH_resp;

// Scan a string. Return a pointer-to-resp tree.
// Caller must free the tree when done, using gth_free_resp()
GTH_resp *gth_parse(const char *string);

// Use this to free the GTH_resp returned by parse_top()
void gth_free_resp(GTH_resp *resp);

// For debugging
void gth_print_tree(GTH_resp *resp);

#endif
