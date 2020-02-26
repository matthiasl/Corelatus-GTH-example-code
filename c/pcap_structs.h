#ifndef PCAP_STRUCTS_H
#define PCAP_STRUCTS_H

// Structures used for reading and writing PCAP files
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2019, Corelatus AB Stockholm
//
// Licence: BSD
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

typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;

//--------------------------------------------------
// PCap classic file format structures.
#pragma pack(push)
#pragma pack(1)
typedef struct {
  u32 magic;
  u16 major_version;
  u16 minor_version;
  u32 GMT_to_localtime;
  u32 sigfigs;
  u32 snaplen;
  u32 network;
} PACK_SUFFIX PCap_classic_global_header;

typedef struct {
  u32 ts_sec;
  u32 ts_us;
  u32 incl_len;
  u32 orig_len;
} PACK_SUFFIX PCap_classic_packet_header;

//--------------------------------------------------
// Pcap-NG file format structures

typedef struct {
  u16 code;
  u16 length;
} PACK_SUFFIX PCap_NG_option;


typedef struct {
  u32 type;
  u32 block_total_length;
  u32 byte_order_magic;
  u16 major_version;
  u16 minor_version;
  unsigned long long section_length;
} PACK_SUFFIX PCap_NG_shb;

typedef struct {
  u32 type;
  u32 block_total_length;
  u16 link_type;
  u16 reserved;
  u32 snaplen;
} PACK_SUFFIX PCap_NG_idb;

typedef struct {
  u32 type;
  u32 block_total_length;
  u32 interface_id;
  u32 timestamp_hi;
  u32 timestamp_lo;
  u32 captured_len;
  u32 packet_len;
} PACK_SUFFIX PCap_NG_epb;

typedef struct {
  u32 type;
  u32 block_total_length;
} PACK_SUFFIX PCap_NG_block;


// The NG40 header is part of reasonably modern (2016) Wireshark versions. It
// lets us put AAL2 in a wireshark file.
//
// http://www.tcpdump.org/linktypes/LINKTYPE_NG40.html
struct NG40_aal2_header {
  u32 type;
  u32 length;
  u32 protocol;
  u32 id;
  u32 flags;
  u8 direction;
  u16 vpi;
  u16 vci;
  u16 cid;
};

// Link types, defined at http://www.tcpdump.org/linktypes.html
//
// See also: http://www.tcpdump.org/linktypes/LINKTYPE_NG40.html
enum Link_type {
  LINK_TYPE_MTP2   = 140,
  LINK_TYPE_MTP3   = 141,
  LINK_TYPE_LAPD   = 203,
  LINK_TYPE_SUNATM = 123,
  LINK_TYPE_NG40   = 244,
  LINK_TYPE_UNKNOWN = 147   // 147 is reserved for "user use"
};

enum Block_type
  {
   PCAPNG_BLOCK_TYPE_IDB = 1,
   PCAPNG_BLOCK_TYPE_EPB = 6
  };
#pragma pack(pop)
#endif
