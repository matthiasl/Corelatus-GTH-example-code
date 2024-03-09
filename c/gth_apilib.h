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
//     gth_connect(&api, "172.16.1.10", 0);
//     gth_new_player(&api, "3A", 16, buffer);
//
// This API supports a subset of the GTH's features.
// Corelatus will extend that subset on request.
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2013, 2009, 2019, Corelatus AB
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
//
//----------------------------------------------------------------------

#include "gth_client_xml_parse.h"

// We promise that a GTH job-id will never be longer than 20 characters.
#define MAX_JOB_ID 20

typedef void(GTH_event_handler)(void *data, GTH_resp *resp);

typedef void(GTH_tone_handler)(const char *name, const int length);

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

  GTH_event_handler *event_handler;
  GTH_tone_handler *tone_handler;
} GTH_api;

// For embedding the version into the object file
extern const char git_head[];
extern const char build_hostname[];

// Close an API connection to the GTH, cleanly.
//
// Return: 0 on success.
int gth_bye(GTH_api *api);

// Make a socket which is connected to the API on a GTH module.
//
// Address is a hostname or a dotted quad, e.g. "172.16.1.10"
// Verbose is boolean
//
// Return: 0 on success
int gth_connect(GTH_api *api, const char *address, const int verbose);

// Make a TCP socket and put it in the listening state.
//
// Return: the socket file descriptor
//
// This function also writes the port number to the *port argument so that
//   you know which port the OS selected.
int gth_make_listen_socket(int *port);

// Return: the socket file descriptor
//
// This function also writes the port number to the *port argument so that
//   you know which port the OS selected.
int gth_make_udp_socket(int *port);

// Given a socket, wait for an accept on it.
//
// Return: the accepted socket.
int gth_wait_for_accept(int listen_socket);

// Wait for a <message ended> event for the given job_id
//
// Return: 0 on success
int gth_wait_for_message_ended(GTH_api *api, const char *job_id);

// Delete a job.
//
// Return: 0 on success
int gth_delete(GTH_api *api, const char *job_id);

// Disable an SDH/SONET or an E1/T1 interface
//
// Return: 0 on success.
int gth_disable(GTH_api *api,
		const char *resource);


// Enable an SDH/SONET or an E1/T1 interface
//
// Return: 0 on success.
int gth_enable(GTH_api *api,
	       const char *resource,
	       const GTH_const_attribute *attributes,
	       int n_attributes);

// Install.
//
// Return: 0 on success
int gth_install(GTH_api *api,
		const char *name,
		const char *type,  // one of "binary/file", "binary/filesystem"
		const char *data,
		const int length); // in octets (bytes)

// Map
//
// Return: 0 on success
// Also writes the resource name to the supplied 'name'
int gth_map(GTH_api *api,
	    const char *resource,
	    char *name,
	    int max_name);

// Start ATM AAL0 monitoring.
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_atm_aal0_monitor(GTH_api *api,
			     const int tag,
			     const char *span,
			     const int timeslots[],
			     const int n_timeslots,
			     char *job_id,
			     const char *ip,
			     const int port);

// gth_atm_aal0_layer is not implemented; it's experimental

// Start ATM AAL2 monitoring.
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_atm_aal2_monitor(GTH_api *api,
			     const int tag,
			     const char *span,
			     const int timeslots[],
			     const int n_timeslots,
			     const int vpi,
			     const int vci,
			     char *job_id,
			     const char *ip,
			     const int port);

// ATM AAL5 from an SDH VC-4 or VC-3
int gth_new_sdh_atm_aal2_monitor(GTH_api *api,
				 const int tag,
				 const char *source,
				 const int vpi,
				 const int vci,
				 char *job_id,
				 const char *ip,
				 const int port);

// Start ATM AAL5 monitoring.
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_atm_aal5_monitor(GTH_api *api,
			     const int tag,
			     const char *span,
			     const int timeslots[],
			     const int n_timeslots,
			     const int vpi,
			     const int vci,
			     char *job_id,
			     const char *ip,
			     const int port);

// ATM AAL5 from an SDH VC-4 or VC-3
int gth_new_sdh_atm_aal5_monitor(GTH_api *api,
				 const int tag,
				 const char *source,
				 const int vpi,
				 const int vci,
				 char *job_id,
				 const char *ip,
				 const int port);


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

// Switch a timeslot from a source to a sink.
//
// This function writes the resulting job-id to job_id
//
// Return: 0 on success
int gth_new_connection(GTH_api *api,
		       const char *src_span,
		       const int   src_ts,
		       const char *dst_span,
		       const int   dst_ts,
		       char *job_id);

// Start transmission of frame relay (or LAPD, or MTP-2) packets
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_fr_layer(GTH_api *api,
                     const char *sink_span,
                     const int timeslots[],
                     const int n_timeslots,
                     char *job_id,
                     const char *ip,
                     const int port);

// Start a LAPD layer.
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by
// calling gth_make_listen_socket() first.
//
// 0 is a reasonable default value for 'sapi' and 'tei'
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_lapd_layer(GTH_api *api,
		       const int tag,
		       const char *span,
		       const int timeslot,
		       const char *side,       // either "network" or "user"
		       const int sapi,
		       const int tei,
		       char *job_id,
		       const char *ip,
		       const int port);

// Start a LAPD monitor with default options.
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_lapd_monitor(GTH_api *api,
			 const int tag,
			 const char *span,
			 const int timeslot,
			 char *job_id,
			 const char *ip,
			 const int port);

// Start a LAPD monitor; with all options possible
//
// Bandwidth can be 16, 32 and 64
// First bit can be 0,2,4,6 for 16 kbit/s and 0,4 for 32 kbit/s.
int gth_new_lapd_monitor_opt(GTH_api *api,
                             const int tag,
                             const char *span,
                             const int ts,
                             const int bandwidth,
                             const int first_bit,
                             char *job_id,
                             const char *ip,
                             const int port,
                             const GTH_attribute *options,
                             const int n_options);

// Start a level detector.
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_level_detector(GTH_api *api,
                           const char *span,
                           const int timeslot,
                           const int threshold,
                           char *job_id,
                           GTH_tone_handler* handler); // callback

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
			 const int timeslots[],
			 const int n_timeslots,
			 char *job_id,
			 const char *ip,
			 const int port);

// Start MTP-2 monitoring, with non-default options.
//
// options: key/value pairs exactly as per the API manual, for instance
//          option.name = "fisu"; option.value = "no".
//
// bandwidth: 64 (normal), 48 (Japan), 56 (North America)
int gth_new_mtp2_monitor_opt(GTH_api *api,
			     const int tag,
			     const char *span,
			     const int timeslots[],
			     const int n_timeslots,
                             const int bandwidth,
			     char *job_id,
			     const char *ip,
			     const int port,
			     const GTH_attribute *options,
			     const int n_options);

// Return: the file descriptor (>= 0) on success.
//
// The file descriptor is a socket to write the player data to.
int gth_new_player(GTH_api *api,
		   const char *span,
		   int timeslot,      // E1: 1--31   T1: 1--24
		   char *job_id);     // function writes the job-id here

// Start raw timeslot monitoring.
//
// The TCP port specified by (ip/port) is expected to be in a
// listening state before entering this function, e.g. by
// calling gth_make_listen_socket() first.
//
// This function writes the resulting job-id to job_id.
//
// Return: 0 on success
int gth_new_raw_monitor(GTH_api *api,
                        const int tag,
                        const char *span,
                        const int timeslots,
                        char *job_id,
                        const char *ip,
                        const int port);

// Return: the file descriptor (>= 0) on success.
//
// The file descriptor is a socket the recorder data gets written to
int gth_new_recorder(GTH_api *api,
		     const char *span,
		     int timeslot,      // E1: 1--31   T1: 1--24
		     char *job_id);     // function writes the job-id here

// Return: 0 on success
//
// The file descriptor is a socket the recorder data gets written to
int gth_new_tone_detector(GTH_api *api,
			  const char *span,
			  int timeslot,      // E1: 1--31   T1: 1--24
			  char *job_id,      // function writes the job-id here
			  GTH_tone_handler* handler); // callback

// Return: the file descriptor (>= 0) on success.
//
// The file descriptor is a UDP socket the incoming data appears on
int gth_new_wide_recorder(GTH_api *api,
			  const char *span,
			  char *job_id);    // function writes the job-id here

// Send a 'no-operation' command to the GTH. Useful for supervision and
// for polling events.
void gth_nop(GTH_api *api);

// Set a attributes on a resource.
//
// Return: 0 on success.
int gth_set(GTH_api *api,
	    const char *resource,
	    const GTH_const_attribute *attributes,
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

// Query a job
//
// 'owner' must point to an array of at least MAX_JOB bytes
//
// This function creates an array of **attributes on the heap; the caller
// is responsible for calling gth_free_attributes() to free them.
//
// *n_attributes is set to the number of attributes returned
//
// Return: 0 on success.
int gth_query_job(GTH_api *api,
                  const char *id,
                  char *owner,
                  GTH_attribute **attributes,
                  int *n_attributes);

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

// Unmap
//
// Return: 0 on success
int gth_unmap(GTH_api *api,
	      const char *resource);

// Send a raw XML command. Intended for debugging only.
//
// Returns a pointer to the response, or 0 if something goes wrong.
GTH_resp *gth_raw_xml(GTH_api *api, const char* string);

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

// Wait for an event on the API socket. Blocks until an event arrives
// or the given timeout expires.
//
// Return: 0 if there's an event
//        -1 on timeout
int gth_wait_for_event(GTH_api *api, const int milliseconds);

// Block until an event arrives, then process it.
//
// Return 0 on success
int gth_process_event(GTH_api *api);

// Make a GTH run the specified software image. If necessary, this
// reboots the GTH.
//
// system_name: either 'failsafe' or 'system'
//     verbose: if nonzero, a couple of progress reports are printed on stderr
void gth_switch_to(const char *hostname,
		   const char *system_name,
		   const int verbose);

// Helper function for manipulating API GTH response trees; mainly useful
// inside a custom event handler.
//
// Return the value of the given attribute in a resp, null if absent.
//
// Does not change the resp at all.
const char *gth_attribute_value(const GTH_resp *resp, const char *key);

// Helper function for manipulating API GTH response trees; mainly useful
// inside a custom event handler.
//
// Returns the value of the given attribute in a resp, null if absent.
//
// Clears the value pointer in the resp, the caller is responsible for
// calling free() on the returned value.
char *gth_attribute_value_and_clear(GTH_resp *resp, const char *key);

// Alternative event handler which silently discards all API events
void gth_silent_event_handler(void *data, GTH_resp *resp);

// Print a timestamp to stderr. Intended for logging.
void gth_print_timestamp(void);

// The Win32 socket library needs some startup actions before it works.
// So call this before doing anything if you're running on win32.
// This function does nothing on non windows platforms.
void win32_specific_startup(void);

// posix and Win32 have different APIs for sleeping. Unify them.
void sleep_seconds(int seconds);

#endif
