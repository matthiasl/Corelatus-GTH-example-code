#ifndef GTH_APILIB_H
#define GTH_APILIB_H

//----------------------------------------------------------------------
// C API for controlling a Corelatus GTH.
//
// Typical use:
//
//     #include "gth_apilib.h"
// 
//     GTH_api api;
//     char buffer[MAX_JOB_ID];
// 
//     gth_connect(&api, "172.16.1.10");
//     gth_new_player(&api, "3A", 16, buffer);
// 
// This API supports a subset of the GTH's features.
// Corelatus will extend that subset on request.
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

#include "gth_client_xml_parse.h"

// We promise that a GTH job-id will never be longer than 20 characters.
#define MAX_JOB_ID 20

typedef void(GTH_event_handler)(void *data, GTH_resp *resp);

// GTH_api is a structure used by all calls to this API code, it's initially
// filled in by gth_connect().
//
// For most uses, consider this structure to be opaque.
// 
// Exceptions to the opaqueness:
//
//     - if you use poll/select to multiplex IO, then you probably want
//       access to the 'fd' field
//
//     - print_cmds and print_responses can be set for debugging
//
//     - if you want to set your own event handler, you can.
//

typedef struct {
  // The file descriptor of the API socket to the GTH
  int fd;

  // Ascii representation of my IP address, from the GTH's pont of view.
  // 16 characters is enough for any IPv4 address, but not IPv6 (IP6).
  char my_ip[16];


  int is_failsafe;      // nonzero means the GTH is currently in failsafe mode.
  int print_cmds;       // nonzero means we want commands to echo on stderr
  int print_responses;  // nonzero means we want responses to echo on stderr

  void *event_handler_data;
  GTH_event_handler *event_handler;
} GTH_api;

// Close an API connection to the GTH, cleanly. 
//
// Return: 0 on success.
int gth_bye(GTH_api *api);

// Make a socket which is connected to the API on a GTH module.
//
// Return: 0 on success
int gth_connect(GTH_api *api, const char *address);

// Make a TCP socket and put it in the listening state.
//
// Return: the socket file descriptor 
//
// This function also writes the port number to the *port argument so that
//   you know which port the OS selected.
int gth_make_listen_socket(int *port);

// Given a socket, wait for an accept on it. 
//
// Return: the accepted socket.
int gth_wait_for_accept(int listen_socket);

// Wait for a <message ended> event for the given job_id
//
// Return: 0 on success
int gth_wait_for_message_ended(GTH_api *api, const char *job_id);

// Print message and abort
void die(const char *message);

// Install.
//
// Return: 0 on success
int gth_install(GTH_api *api,
		const char *name,
		const char *type,  // one of "binary/file", "binary/filesystem"
		const char *data,
		const int length); // in octets (bytes)

// Start CAS MFC monitoring. 
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by 
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
// 
// Return: 0 on success
int gth_new_cas_r2_mfc_detector(GTH_api *api,
				const int tag,
				const char *span,
				int timeslot,
				char *job_id,
				const char *ip,
				const int port);

// Same as gth_new_cas_r2_mfc_detector(), but for line signalling.
int gth_new_cas_r2_linesig_monitor(GTH_api *api,
				   const int tag,
				   const char *span,
				   const int timeslot,
				   char *job_id,
				   const char *ip,
				   const int port);


// Start MTP-2 monitoring. 
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by 
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
// 
// Return: 0 on success
int gth_new_mtp2_monitor(GTH_api *api,
			 const int tag,
			 const char *span,
			 const int timeslot,
			 char *job_id,
			 const char *ip,
			 const int port);


// Return: the file descriptor (>= 0) on success. 
//
// The file descriptor is a socket to write the player data to.
int gth_new_player(GTH_api *api, 
		   const char *span, 
		   int timeslot,      // E1: 1--31   T1: 1--24
		   char *job_id);     // function writes the job-id here
		   
// Return: the file descriptor (>= 0) on success. 
//
// The file descriptor is a socket the recorder data gets written to
int gth_new_recorder(GTH_api *api, 
		     const char *span, 
		     int timeslot,      // E1: 1--31   T1: 1--24
		     char *job_id);     // function writes the job-id here
		   

// Set a attributes on a resource.
//
// Return: 0 on success.
int gth_set(GTH_api *api,
	    const char *resource,
	    const GTH_attribute *attributes,
	    int n_attributes);

// Special case of set for just one attribute.
//
// Return: 0 on success.
int gth_set_single(GTH_api *api,
		   const char *resource,
		   const char *attribute,
		   const char *value);

// Reset. The only valid resource is "cpu".
int gth_reset(GTH_api *api, const char *resource);

// Query one attribute on a resource. Returns the value in result, using up
// to max_size characters, including the terminating 0.
//
// On error, result is set to the empty string and the function 
// return value is nonzero.
int gth_query_resource_attribute(GTH_api *api, 
				 const char *resource, 
				 const char *attribute,
				 char *result,
				 int max_size);

// Query a resource. 
//
// **attributes is set to an array of attributes. The caller is responsible
// for calling gth_free_attributes() to free the attributes.
//
// *n_attributes is set to the number of attributes returned
//
// Return: 0 on success.
//
int gth_query_resource(GTH_api *api,
		       const char *name,
		       GTH_attribute **attributes,
		       int *n_attributes);				    

// Free an array of attributes, typically obtained from gth_query_resource()
void gth_free_attributes(GTH_attribute *attributes, int n_attributes);

// Return an ascii representation of this machine's IP address as 
// observed by the GTH.
const char *gth_my_ip_address(GTH_api *api);

// Wait for a GTH to reboot. Assumes that the GTH has just been 
// given a boot command or just plugged in.
//
// Return: 0 on success
//        -1 on timeout
int gth_wait_for_reboot(const char *hostname);

// Make a GTH run the specified software image. If necessary, this
// reboots the GTH.
//
// system_name: either 'failsafe' or 'system'
//     verbose: if nonzero, a couple of progress reports are printed on stderr
void gth_switch_to(const char *hostname, 
		   const char *system_name, 
		   const int verbose);

// The Win32 socket library needs some startup actions before it works.
// So call this before doing anything if you're running on win32.
// This function does nothing on non windows platforms.
void win32_specific_startup();

#endif
