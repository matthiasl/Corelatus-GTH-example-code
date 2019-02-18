//----------------------------------------------------------------------
// API routines for talking to a Corelatus GTH.
//
// If you are already familiar with the GTH API, then looking at gth_apilib.h
// and some of the examples, e.g. record.c, is one way to get started.
//
// If you aren't familiar with the GTH API, see www.corelatus.com/gth/api/
//
// Documentation for this module is in the header file (gth_apilib.h)
//
// Author: Matt Lang (matthias@corelatus.se)
//
// Copyright (c) 2013, 2009, Corelatus AB Stockholm
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
#include <errno.h>
#include <string.h>
#include <assert.h>
#include <time.h>

#ifdef WIN32
#include <winsock2.h>
typedef int socklen_t;
#else
#include <unistd.h>
#include <time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/select.h>
#include <fcntl.h>
#endif // WIN32

#include "gth_win32_compat.h"
#include "gth_apilib.h"
#include "gth_client_xml_parse.h"

#define MAX_COMMAND 10000
#define MAX_RESPONSE 10000
#define MAX_LOGFILE 20000000

const int GTH_API_PORT = 2089;        // TCP port a Corelatus GTH listens on

#ifndef WIN32   // i.e. unix.

// strncpy_s is provided by Microsoft, but not GNU.
// This implementation is meant to do the same as Microsoft's, except
//    - no support for _TRUNCATE
//    - no "invalid parameter handler"
int
strncpy_s(char *dest,
	  size_t dest_size,
	  const char *src,
	  size_t copy_count)
{
  size_t x;

  if (!dest) return EINVAL;
  if (dest_size == 0) return EINVAL;

  if (!src)
    {
      dest[0] = 0;
      return EINVAL;
    }

  for (x = 0; x < copy_count && x < dest_size - 1 && src[x] != 0; x++)
      dest[x] = src[x];

  if (x >= dest_size)
    {
      dest[0] = 0;
      return ERANGE;
    }

  dest[x] = 0;

  return 0;
}

// asctime_s is provided by Microsoft, in such a way that the easiest way
// to make it work on both *nix and Microsoft is to use Microsoft's
// naming scheme.
int asctime_s(char *dest, size_t dest_size, const struct tm *time)
{
  char *result;
  result = asctime_r(time, dest);
  assert(dest_size >= 26);  // 'asctime' manpage requires this
  return (result == 0 /* asctime_r returns a pointer, or NULL */);
}

void set_nonblocking(int s, int on_or_off) // 1 means nonblocking
{
  int flags = on_or_off?O_NONBLOCK:0;
  int result = fcntl(s, F_SETFL, flags);
  assert(result == 0);
}
#else
void set_nonblocking(int s, int on_or_off) // 1 means nonblocking
{
  int result;
  u_long mode = on_or_off;

  result = ioctlsocket(s, FIONBIO, &mode);
  assert(result == 0);
}
#endif




//----------------------------------------------------------------------
// Forward declarations.

static int read_header_line(int fd, char *line);
static int definite_read(int fd,
			 int length,
			 char *response,
			 const int max_response_length);
static GTH_resp *gth_next_non_event(GTH_api *api);
static void string_write(int s, const char* string);
static int query_single_resource(GTH_api *api,
				 const char *name,
				 GTH_attribute **attributes,
				 int *n_attributes);
static int query_inventory(GTH_api *api,
			   GTH_attribute **attributes,
			   int *n_attributes);

static void my_ip_address(GTH_api *api);

static int recv_job_id(GTH_api *api, char *id);
static void api_write(GTH_api *api, const char* command);
static void api_write_non_xml(int s,
				  const char* type,
				  const char* data,
				  int len);

static int new_atm_aal_monitor(GTH_api *api,
                               const int tag,
                               const char *span,
                               const int timeslots[],
                               const int n_timeslots,
                               const int vpi,
                               const int vci,
                               char *job_id,
                               const char *ip,
                               const int port,
                               const int aal);


static int new_sdh_atm_aal_monitor(GTH_api *api,
                                   const int tag,
                                   const char *source,
                                   const int vpi,
                                   const int vci,
                                   char *job_id,
                                   const char *ip,
                                   const int port,
                                   const int aal);

// Read the next GTH response from the given API connection.
//
// The response is written to the caller-provided response buffer,
// truncated to response_length, including the zero termination.
//
// Return: 0 on success, anything else is an error
//
static int next_api_response(GTH_api *api,
			     char* response,
			     const int max_response_length);

// Use the XML parser to parse the next response
//
// Return 0 if the response was as expected
//
// If the response is not as expected and **actual is non-null,
// the actual response is left in **actual. The caller must then free it.
//
static int check_api_response(GTH_api *api, GTH_resp_type expected,
			      GTH_resp **actual);

//----------------------------------------------------------------------

void die(const char* message)
{
  fprintf(stderr, "%s\n", message);

  // UAC on Windows 7 (and possibly other variants) may run the
  // program in a new window. This makes errors disappear if
  // we exit immediately. So delay.
  #ifdef WIN32
  fprintf(stderr, "Press ^c (or wait 30 seconds)\n");
  sleep_seconds(30);
  #endif

  exit(-1);
}


void *checked_realloc(void *ptr, size_t size)
{
  void *result;

  result = realloc(ptr, size);

  if (!result) die("realloc failed. Out of memory?");

  return result;
}

void *checked_malloc(size_t size)
{
  void *result;

  result = malloc(size);

  if (!result) die("malloc failed. Out of memory?");

  return result;
}

void gth_print_timestamp()
{
  char timestring[50];  // manpage promises max 26 bytes
  char *nl;
  time_t timestamp;
  int result;

  timestamp = time(0);
  result = asctime_s(timestring, sizeof timestring, gmtime(&timestamp));
  if (result != 0) die("asctime failed");
  nl = strrchr(timestring, '\n');
  if (nl) *nl = ' ';

  fputs(timestring, stderr);
}


void gth_event_handler(void *data, GTH_resp *resp)
{
  GTH_resp *child;
  GTH_api *api = data;

  assert(api);
  assert(resp);
  assert(resp->type == GTH_RESP_EVENT);
  assert(resp->n_children == 1);

  child = resp->children + 0;

  switch (child->type) {

  case GTH_RESP_INFO: {
    const char *reason = gth_attribute_value(child, "reason");

    if (!strcmp(reason, "failsafe_mode")) {
      api->is_failsafe = 1;
    } else {
      fprintf(stderr, "Ignoring an <info> with reason=%s\n", reason);
    }
    break;
  }

  case GTH_RESP_ATM_MESSAGE:      // fall through
  case GTH_RESP_F_RELAY_MESSAGE:  // fall through
  case GTH_RESP_L1_MESSAGE:       // fall through
  case GTH_RESP_LAPD_MESSAGE:     // fall through
  case GTH_RESP_MTP2_MESSAGE:     // fall through
  case GTH_RESP_SYNC_MESSAGE: {
    gth_print_timestamp();
    gth_print_tree(resp);
    break;
  }

  case GTH_RESP_TONE:
    if (api->tone_handler) {
      const char *name = gth_attribute_value(child, "name");
      const char *length = gth_attribute_value(child, "length");
      api->tone_handler(name, atoi(length));
    }
    break;

  case GTH_RESP_LEVEL:
    if (api->tone_handler) {
      const char *id = gth_attribute_value(child, "detector");
      const char *state = gth_attribute_value(child, "state");
      api->tone_handler(id, strcmp(state, "noisy") == 0);
    }
    break;

    // no handler -> fall through to printing the tone event
  default:
    gth_print_timestamp();
    fprintf(stderr,
	    "gth_event_handler got an event, handling with default handler\n");
    gth_print_tree(resp);
    break;
  }

  // do not free the resp, it's not yours to free. (handlers may be chained)
}


int gth_connect(GTH_api *api, const char *address, const int verbose)
{
  struct sockaddr_in gth_addr;
  struct hostent* host;

  assert(api);
  api->is_failsafe = 0;
  api->print_cmds = verbose;
  api->print_responses = verbose;
  api->event_handler = &gth_event_handler;
  api->tone_handler = 0;

  host = gethostbyname(address);

  if (host == 0) {
    return -ENETUNREACH;
  }

  if (host->h_addr_list[0] == 0) {
    return -ENETUNREACH;
  }

  gth_addr.sin_family = AF_INET;
  gth_addr.sin_port = htons(GTH_API_PORT);
  memcpy(&gth_addr.sin_addr, host->h_addr_list[0], host->h_length);

  api->fd = socket(PF_INET, SOCK_STREAM, 0);
  if (api->fd < 0) {
    return -ENOTSOCK;
  }

  if (connect(api->fd, (struct sockaddr *)&gth_addr, sizeof(gth_addr))) {
    // On *nix, errno tells us why the connect failed.
    // On Win32, you have to call some WSA function.
    return -1;
  }

  my_ip_address(api);

  return 0;
}

// Helper for simple commands.
static int single_arg_ok_response(GTH_api *api,
				  const char *template,
				  const char *arg)
{
  char buffer[MAX_COMMAND];

  assert(api);
  assert(template);
  assert(arg);

  snprintf(buffer, MAX_COMMAND, template, arg);
  api_write(api, buffer);

  if (check_api_response(api, GTH_RESP_OK, 0)) {
    return -1;
  }

  return 0;
}

int gth_bye(GTH_api *api)
{
  return single_arg_ok_response(api, "<bye/>%s", "");
}

// Delete a job.
//
// Return: 0 on success
int gth_delete(GTH_api *api, const char *job_id)
{
  return single_arg_ok_response(api, "<delete id='%s'/>", job_id);
}

int gth_make_listen_socket(int* port)
{
  int s = -1;
  int result;
  struct sockaddr_in addr;
  socklen_t addr_size = sizeof addr;

  addr.sin_family = AF_INET;
  addr.sin_port = 0;
  addr.sin_addr.s_addr = INADDR_ANY;

  s = socket(PF_INET, SOCK_STREAM, 0);
  assert(s >= 0);

  result = bind(s, (struct sockaddr*)&addr, sizeof addr);
  assert(result == 0);

  result = listen(s, 1);
  assert(result == 0);

  // Set listen sockets to nonblocking. Avoids a race condition: if
  // the network dies after select() returns but before we call
  // accept(), accept will unexpectedly block.
  set_nonblocking(s, 1);

  result = getsockname(s, (struct sockaddr*)&addr, &addr_size);
  assert(result == 0);
  assert(addr_size == sizeof addr);

  *port = ntohs(addr.sin_port);
  return s;
}

int gth_make_udp_socket(int* port)
{
  int s = -1;
  int result;
  struct sockaddr_in addr;
  socklen_t addr_size = sizeof addr;

  addr.sin_family = AF_INET;
  addr.sin_port = 0;
  addr.sin_addr.s_addr = INADDR_ANY;

  s = socket(PF_INET, SOCK_DGRAM, 0);
  assert(s >= 0);

  result = bind(s, (struct sockaddr*)&addr, sizeof addr);
  assert(result == 0);

  result = getsockname(s, (struct sockaddr*)&addr, &addr_size);
  assert(result == 0);
  assert(addr_size == sizeof addr);

  *port = ntohs(addr.sin_port);
  return s;
}


int gth_wait_for_accept(int listen_socket)
{
  int data_socket = -1;
  int result;
  struct timeval timeout = {2, 0};  // 2 seconds
  fd_set readfds;

  FD_ZERO(&readfds);
  FD_SET(listen_socket, &readfds);

  result = select(listen_socket + 1, &readfds, 0, 0, &timeout);

  if (result == 0)
    die("unable to accept socket, timed out (is this host firewalled?)");

  if (result < 0)
    die("unabled to accept socket");

  data_socket = accept(listen_socket, 0, 0);
  if (data_socket < 0) {
    die("unable to accept socket (possibly blocked by a firewall)");
  }

  set_nonblocking(data_socket, 0);

  return data_socket;
}

// Return 1 if the given resp is an 'install_done' event, 0 otherwise.
static int is_install_done_event(GTH_resp *resp)
{
    GTH_resp *child;

    if (resp->type != GTH_RESP_EVENT)
      return 0;

    assert(resp->n_children == 1);

    child = resp->children + 0;

    if (child->type == GTH_RESP_INFO) {
      if (!strcmp("install_done", gth_attribute_value(child, "reason"))) {
	return 1;
      }
    }

    return 0;
}


// Return: 0 on success
static int gth_wait_for_install_complete(GTH_api *api, int need_install_done)
{
  char buffer[MAX_COMMAND];
  int result;
  GTH_resp *resp = 0;
  int looking_for_ok = 1;
  int looking_for_install_done = need_install_done;

  assert(api);

  for (;;) {
    result = next_api_response(api, buffer, sizeof(buffer));
    if (result != 0) {
      return -1;
    }

    resp = gth_parse(buffer);

    if (resp == 0) {
      return -1;
    }

    if (is_install_done_event(resp))
      looking_for_install_done = 0;
    else
      {
	assert(api->event_handler);
	if (resp->type == GTH_RESP_EVENT)
	  (*api->event_handler)(api, resp);
	else if (resp->type == GTH_RESP_OK)
	  looking_for_ok = 0;
	else
	  {
	    gth_print_tree(resp);
	    gth_free_resp(resp);
	    return -1;
	  }
      }

    gth_free_resp(resp);

    if (!looking_for_install_done && !looking_for_ok)
      return 0;
  }

  return 0;
}

static int kv_to_tags(char *buffer,
		      const size_t buflen,
		      const GTH_attribute *attributes,
		      int n)
{
  int used = 0;

  *buffer = 0;

  while (attributes && n > 0) {
    used += snprintf(buffer + used, buflen - used,
		     "<attribute name='%s' value='%s'/>",
		     attributes->key, attributes->value);
    n--;
    attributes++;
  }

  return used;
}

static int kv_to_attributes(char *buffer,
			    const size_t buflen,
			    const GTH_attribute *attributes,
			    int n)
{
  int used = 0;

  *buffer = 0;

  while (attributes && n > 0) {
    used += snprintf(buffer + used, buflen - used,
		     " %s='%s'",
		     attributes->key, attributes->value);
    n--;
    attributes++;
  }

  return used;
}

static void format_sources(const char *span,
			   const int timeslots[],
			   const int n_timeslots,
                           const int bandwidth,
			   char *sources)
{
  char *pos = sources;
  char *template;
  int result;
  int x;

  switch (bandwidth) {
  case 64: template = "<pcm_source span='%s' timeslot='%d'/>";
    break;
  case 56: template = "<pcm_source span='%s' timeslot='%d' bandwidth='56'/>";
    break;
  case 48: template = "<pcm_source span='%s' timeslot='%d' bandwidth='48' first_bit='1'/>";
    break;
  default: die("invalid bandwidth, must be 48, 56 or 64");
  }


  for (x = 0; x < n_timeslots; x++)
    {
      result = snprintf(pos, MAX_COMMAND - (pos - sources), template,
			span, timeslots[x]);
      pos += result;
      assert(result < (MAX_COMMAND - (pos - sources)));
    }
}


// Internal; used by both gth_enable and gth_set
static int gth_enable_or_set(const char *command,
			     GTH_api *api,
			     const char *resource,
			     const GTH_attribute *attributes,
			     int n_attributes)
{
  char buffer[MAX_COMMAND];
  int used;

  assert(api);
  assert(resource);

  used = snprintf(buffer, MAX_COMMAND, "<%s name='%s'>", command, resource);

  used += kv_to_tags(buffer + used, MAX_COMMAND - used,
		     attributes, n_attributes);

  used += snprintf(buffer + used, MAX_COMMAND - used, "</%s>", command);

  api_write(api, buffer);

  if (check_api_response(api, GTH_RESP_OK, 0)) {
    return -1;
  }

  return 0;
}

int gth_disable(GTH_api *api,
		const char *resource)
{
  return single_arg_ok_response(api, "<disable name='%s'/>", resource);
}

int gth_enable(GTH_api *api,
	       const char *resource,
	       const GTH_attribute *attributes,
	       int n_attributes)
{
  return gth_enable_or_set("enable", api, resource, attributes, n_attributes);
}

int gth_install(GTH_api *api,
		const char *name,
		const char *type,
		const char *data,
		const int length)
{
  char buffer[MAX_COMMAND];
  int result;
  int is_firmware_install = !strcmp(type, "binary/filesystem");

  assert(api);

  snprintf(buffer, MAX_COMMAND, "<install name='%s'/>", name);
  api_write(api, buffer);
  api_write_non_xml(api->fd, type, data, length);

  result = gth_wait_for_install_complete(api, is_firmware_install);

  return result;
}

int gth_new_atm_aal0_monitor(GTH_api *api,
			     const int tag,
			     const char *span,
			     const int timeslots[],
			     const int n_timeslots,
                             char *job_id,
			     const char *ip,
			     const int port)
{
  char command[MAX_COMMAND];
  char sources[MAX_COMMAND];
  int result;
  const char* template;

  assert(n_timeslots < 32 && n_timeslots > 0);

  template = "<new><atm_aal0_monitor ip_addr='%s' ip_port='%d' tag='%d'>"
    "%s</atm_aal0_monitor></new>";

  format_sources(span, timeslots, n_timeslots, 64, sources);

  result = snprintf(command, MAX_COMMAND, template, ip, port, tag, sources);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  return result;
}


int gth_new_atm_aal2_monitor(GTH_api *api,
			     const int tag,
			     const char *span,
			     const int timeslots[],
			     const int n_timeslots,
			     const int vpi,
			     const int vci,
			     char *job_id,
			     const char *ip,
			     const int port)
{
  return new_atm_aal_monitor(api, tag, span, timeslots, n_timeslots,
                             vpi, vci, job_id, ip, port, 2);
}

int gth_new_sdh_atm_aal2_monitor(GTH_api *api,
				 const int tag,
				 const char *source,
				 const int vpi,
				 const int vci,
				 char *job_id,
				 const char *ip,
				 const int port)
{
  return new_sdh_atm_aal_monitor(api, tag, source,
                                 vpi, vci, job_id, ip, port, 2);
}

int gth_new_atm_aal5_monitor(GTH_api *api,
			     const int tag,
			     const char *span,
			     const int timeslots[],
			     const int n_timeslots,
			     const int vpi,
			     const int vci,
			     char *job_id,
			     const char *ip,
			     const int port)
{
  return new_atm_aal_monitor(api, tag, span, timeslots, n_timeslots,
                             vpi, vci, job_id, ip, port, 5);
}

int gth_new_sdh_atm_aal5_monitor(GTH_api *api,
				 const int tag,
				 const char *source,
				 const int vpi,
				 const int vci,
				 char *job_id,
				 const char *ip,
				 const int port)
{
  return new_sdh_atm_aal_monitor(api, tag, source,
                                 vpi, vci, job_id, ip, port, 5);
}

int gth_new_cas_r2_mfc_detector(GTH_api *api,
				const int tag,
				const char *span,
				int timeslot,    // E1: 1--31   T1: 1--24
				char *job_id,    // function writes to job-id
				const char* ip,
				const int port)
{
  int result;
  char command[MAX_COMMAND];
  const char* command_template =
    "<new><cas_r2_mfc_detector tag='%d' direction='forward' "
    "ip_addr='%s' ip_port='%d'><pcm_source span='%s' timeslot='%d'/>"
    "</cas_r2_mfc_detector></new>";

  snprintf(command, MAX_COMMAND, command_template,
	   tag, ip, port, span, timeslot);
  api_write(api, command);

  result = recv_job_id(api, job_id);

  return result;
}

int gth_new_cas_r2_linesig_monitor(GTH_api *api,
				   const int tag,
				   const char *span,
				   const int ts,
				   char *job_id,
				   const char* ip,
				   const int port)
{
  int result;
  char command[MAX_COMMAND];
  const char* command_template =
    "<new><cas_r2_linesig_monitor tag='%d' "
    "ip_addr='%s' ip_port='%d'><pcm_source span='%s' timeslot='%d'/>"
    "</cas_r2_linesig_monitor></new>";

  snprintf(command, MAX_COMMAND, command_template, tag, ip, port, span, ts);
  api_write(api, command);

  result = recv_job_id(api, job_id);

  return result;
}

int gth_new_connection(GTH_api *api,
		       const char *src_span,
		       const int   src_ts,
		       const char *dst_span,
		       const int   dst_ts,
		       char *job_id)
{
  int result;
  char command[MAX_COMMAND];
  const char* command_template =
    "<new><connection>"
    "<pcm_source span='%s' timeslot='%d'/>"
    "<pcm_sink   span='%s' timeslot='%d'/>"
    "</connection></new>";

  snprintf(command, MAX_COMMAND, command_template, src_span, src_ts,
	   dst_span, dst_ts);
  api_write(api, command);

  result = recv_job_id(api, job_id);

  return result;
}

int gth_new_lapd_layer(GTH_api *api,
		       const int tag,
		       const char *span,
		       const int ts,
		       const char *side,       // either "network" or "user"
		       const int sapi,
		       const int tei,
		       char *job_id,
		       const char *ip,
		       const int port)
{
  char command[MAX_COMMAND];
  int result;
  const char* template;

  assert(ts > 0 && ts < 32);
  assert(!strcmp("network", side) || !strcmp("user", side));

  template = "<new><lapd_layer side='%s' sapi='%d' tei='%d' ip_addr='%s' ip_port='%d' tag='%d'>"
    "<pcm_source span='%s' timeslot='%d'/>"
    "<pcm_sink span='%s' timeslot='%d'/>"
    "</lapd_layer></new>";

  result = snprintf(command, MAX_COMMAND, template, side, sapi,
		    tei, ip, port, tag, span, ts, span, ts);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  return result;
}


int gth_new_lapd_monitor(GTH_api *api,
			 const int tag,
			 const char *span,
			 const int ts,
			 char *job_id,
			 const char *ip,
			 const int port)
{
  char command[MAX_COMMAND];
  int result;
  const char* template;

  assert(ts > 0 && ts < 32);

  template = "<new><lapd_monitor ip_addr='%s' ip_port='%d' tag='%d'>"
    "<pcm_source span='%s' timeslot='%d'/>"
    "</lapd_monitor></new>";

  result = snprintf(command, MAX_COMMAND, template, ip, port, tag, span, ts);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  return result;
}


int gth_new_level_detector(GTH_api *api,
                           const char *span,
                           const int ts,
                           const int threshold,
                           char *job_id,
                           GTH_tone_handler* handler)
{
  char command[MAX_COMMAND];
  int result;
  const char* template;

  assert(ts > 0 && ts < 32);

  template = "<new><level_detector threshold='%d'>"
    "<pcm_source span='%s' timeslot='%d'/>"
    "</level_detector></new>";

  result = snprintf(command, MAX_COMMAND, template, threshold, span, ts);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  if (result == 0) {
    api->tone_handler = handler;
  }

  return result;
}


int gth_new_mtp2_monitor(GTH_api *api,
			 const int tag,
			 const char *span,
			 const int timeslots[],
			 const int n_timeslots,
			 char *job_id,
			 const char *ip,
			 const int port)
{
  return gth_new_mtp2_monitor_opt(api, tag, span, timeslots, 64,
				  n_timeslots, job_id, ip, port, 0, 0);
}


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
			     const int n_options)
{
  char command[MAX_COMMAND];
  char sources[MAX_COMMAND];
  char attributes[MAX_COMMAND];
  int result;
  const char* template;

  assert(n_timeslots < 32 && n_timeslots > 0);

  result = kv_to_attributes(attributes, MAX_COMMAND, options, n_options);

  template = "<new><mtp2_monitor %s ip_addr='%s' ip_port='%d' tag='%d'>"
    "%s</mtp2_monitor></new>";

  format_sources(span, timeslots, n_timeslots, bandwidth, sources);

  result = snprintf(command, MAX_COMMAND, template,
		    attributes, ip, port, tag, sources);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  return result;
}


int gth_new_player(GTH_api *api,
		   const char *span,
		   int timeslot,
		   char *job_id)
{
  int listen_port = 0;
  int listen_socket = gth_make_listen_socket(&listen_port);
  int data_socket;
  char command[MAX_COMMAND];
  int result;
  const char* template;

  assert(api);
  assert(span);
  assert(job_id);
  assert(timeslot > 0 && timeslot < 32);

  template = "<new><player><tcp_source ip_addr='%s' ip_port='%d'/>"
    "<pcm_sink span='%s' timeslot='%d'/></player></new>";

  result = snprintf(command, MAX_COMMAND, template,
		    api->my_ip, listen_port, span, timeslot);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);
  if (result == 0) {
    data_socket = gth_wait_for_accept(listen_socket);
  }
  closesocket(listen_socket);

  return (result == 0)?data_socket:-1;
}


int gth_new_raw_monitor(GTH_api *api,
                        const int tag,
                        const char *span,
                        const int timeslot,
                        char *job_id,
                        const char *ip,
                        const int port)
{
  char command[MAX_COMMAND];
  int result;
  const char* template;

  template = "<new><raw_monitor ip_addr='%s' ip_port='%d' tag='%d'>"
    "<pcm_source span='%s' timeslot='%d'/></raw_monitor></new>";

  result = snprintf(command, MAX_COMMAND, template,
                    ip, port, tag, span, timeslot);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  return result;
}


int gth_new_recorder(GTH_api *api,
		     const char *span,
		     int timeslot,
		     char *job_id)
{
  int listen_port = 0;
  int listen_socket = gth_make_listen_socket(&listen_port);
  int data_socket;
  char command[MAX_COMMAND];
  int result;
  const char* template;

  assert(api);
  assert(span);
  assert(timeslot > 0 && timeslot < 32);
  assert(job_id);

  template = "<new><recorder><pcm_source span='%s' timeslot='%d'/>"
    "<tcp_sink ip_addr='%s' ip_port='%d'/>"
    "</recorder></new>";

  result = snprintf(command, MAX_COMMAND, template,
		    span, timeslot, api->my_ip, listen_port);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);
  if (result == 0) {
    data_socket = gth_wait_for_accept(listen_socket);
  }
  closesocket(listen_socket);

  return (result == 0)?data_socket:-1;
}

int gth_new_tone_detector(GTH_api *api,
			  const char *span,
			  int timeslot,
			  char *job_id,
			  GTH_tone_handler* handler)
{
  const char *template;
  int result;
  char command[MAX_COMMAND];

  template = "<new><tone_detector><pcm_source span='%s' timeslot='%d'/>"
    "</tone_detector></new>";

  result = snprintf(command, MAX_COMMAND, template, span, timeslot);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  if (result == 0) {
    api->tone_handler = handler;
  }

  return result;
}


void gth_nop(GTH_api *api)
{
  GTH_resp *resp;

  api_write(api, "<nop/>");

  resp = gth_next_non_event(api);

  if (!resp || resp->type != GTH_RESP_OK) {
    die("nop failed. Aborting.");
  }

  gth_free_resp(resp);
}

int gth_new_wide_recorder(GTH_api *api,
			  const char *span,
			  char *job_id)
{
  int portno = 0;
  int data_socket = gth_make_udp_socket(&portno);
  char command[MAX_COMMAND];
  int result;
  const char* template;

  assert(api);
  assert(span);
  assert(job_id);

  template = "<new><wide_recorder span='%s'>"
    "<udp_sink ip_addr='%s' ip_port='%d'/>"
    "</wide_recorder></new>";

  result = snprintf(command, MAX_COMMAND, template,
		    span, api->my_ip, portno);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);
  if (result != 0) {
    closesocket(data_socket);
  }

  return (result == 0)?data_socket:-1;
}


// We're expecting a <message_ended> event, wait for it.
// Any other messages which arrive go to the normal event handler.
int gth_wait_for_message_ended(GTH_api *api, const char *job_id)
{
  char buffer[MAX_COMMAND];
  int result;
  GTH_resp *resp = 0;

  assert(api);
  assert(job_id);

  for (;;) {
    GTH_resp *child;
    const char *event_job_id;

    result = next_api_response(api, buffer, sizeof(buffer));
    if (result != 0) {
      return 0;
    }
    resp = gth_parse(buffer);

    assert(resp);
    assert(resp->type == GTH_RESP_EVENT);
    assert(resp->n_children == 1);

    child = resp->children + 0;

    if (child->type == GTH_RESP_MESSAGE_ENDED) {
      event_job_id = gth_attribute_value(child, "id");
      if (!strcmp(event_job_id, job_id)) {
	gth_free_resp(resp);
	return 0;
      }
    }

    assert(api->event_handler);
    (*api->event_handler)(api, resp);
    gth_free_resp(resp);
  }

  return 0;
}

// There's a job ID coming back on the command socket. Parse it.
//
// Return: 0 on success  (and the id in *id, possibly truncated)
static int recv_job_id(GTH_api *api, char *id)
{
  GTH_resp *resp;
  const char *id_attr;
  int result = 0;

  assert(api);
  assert(id);

  resp = gth_next_non_event(api);
  if (resp == 0) return -1;

  if (resp->type == GTH_RESP_JOB)
    {
      id_attr = gth_attribute_value(resp, "id");
      strncpy_s(id, MAX_JOB_ID, id_attr, MAX_JOB_ID - 1);
    }
  else
    {
      result = -2;
    }

  gth_free_resp(resp);

  return result;
}

// 80 characters is guaranteed to be enough for both header lines
#define GTH_HEADER_BUFFER_LEN 80

// Read a header line. A header line is always terminated by \r\n, though
// we only check for \r here.
static int read_header_line(int fd, char *line) {
  int used = 0;
  int result;

  do {
    result = recv(fd, line + used, 1, 0);
    used++;
    if (used >= GTH_HEADER_BUFFER_LEN || result != 1) {
      return -1;
    }
  } while (line[used-1] != '\r');

  line[used-1] = 0;

  // eat up the \n
  result = recv(fd, line + used, 1, 0);

  if (result != 1) {
    return -1;
  }

  return 0;
}

//----------------------------------------------------------------------
// Read exactly 'length' bytes from the given fd
//
// Store 'max_response_length' - 1 of the bytes in the 'response' buffer,
//   which is null-terminated
//
// Return: 0 on success
//
static int definite_read(int fd,
			 int length,
			 char *response,
			 const int max_response_length)
{
  size_t to_read;
  ssize_t result;

  to_read = length;
  if (max_response_length <= length) {
    to_read = max_response_length - 1;
  }

  length -= to_read;

  while (to_read > 0) {
    result = recv(fd, response, to_read, 0);
    if (result <= 0) {
      closesocket(fd);
      return -9;
    }
    response += result;
    to_read -= result;
  }

  *response = 0;

  if (length == 0) {
    return 0;
  }

  while (length > 0) {
    char internal_buffer[4000];
    to_read = length;
    if (to_read > sizeof(internal_buffer)) {
      to_read = sizeof(internal_buffer);
    }
    result = recv(fd, internal_buffer, to_read, 0);
    length -= to_read;
  }

  return -10;
}

//----------------------------------------------------------------------
static int next_api_response(GTH_api *api,
			     char *response,
			     const int max_response_length)
{
  const char *key1 = "Content-type: ";
  const char *key2 = "Content-length: ";
  char content_type[GTH_HEADER_BUFFER_LEN];
  char content_length[GTH_HEADER_BUFFER_LEN];
  char type[GTH_HEADER_BUFFER_LEN];
  int length;
  int result1;
  int result2;

  assert(api);
  assert(response);

  result1 = read_header_line(api->fd, content_type);
  result2 = read_header_line(api->fd, content_length);
  if (result1 != 0 || result2 != 0) {
    closesocket(api->fd);
    api->fd = -1;
    return -1;
  }

  if (strstr(content_type,   key1) != content_type ||
      strstr(content_length, key2) != content_length) {
    closesocket(api->fd);
    api->fd = -1;
    return -2;
  }

  strncpy_s(type,
	    GTH_HEADER_BUFFER_LEN,
	    content_type + strlen(key1),
	    GTH_HEADER_BUFFER_LEN - 1);
  length = atoi(content_length + strlen(key2));

  if (strcmp(type, "text/xml") != 0 && strcmp(type, "text/plain") != 0) {
    closesocket(api->fd);
    api->fd = -1;
    return -3;
  }

  // flush the blank line
  result1 = read_header_line(api->fd, content_type);
  if (result1 != 0) {
    closesocket(api->fd);
    api->fd = -1;
    return -4;
  }

  result2 = definite_read(api->fd, length, response, max_response_length);

  if (api->print_responses) {
    fprintf(stderr, "GTH response: %s\n", response);
  }

  return result2;
}

static int check_api_response(GTH_api *api, GTH_resp_type expected,
			      GTH_resp **actual)
{
  GTH_resp *resp;
  int same;

  assert(api);

  resp = gth_next_non_event(api);

  if (!resp) {
    if (actual) {
      actual = 0;
    }
    return -1;
  }

  if (resp->type != expected && actual) {
    *actual = resp;
    return -2;
  }

  same = (resp->type == expected);
  gth_free_resp(resp);

  return !same;
}

// Return the next response which is _not_ an event
static GTH_resp *gth_next_non_event(GTH_api *api) {
  char buffer[MAX_RESPONSE];
  int result;
  GTH_resp *resp = 0;

  assert(api);

  for (;;) {
    result = next_api_response(api, buffer, sizeof(buffer));

    if (result != 0)
      {
	return 0;
      }

    resp = gth_parse(buffer);

    if (!resp)
      {
	return 0;
      }

    if (resp->type != GTH_RESP_EVENT)
      {
	return resp;
      }

    assert(api->event_handler);
    (*(api->event_handler))(api, resp);
    gth_free_resp(resp);
  }
}

static void string_write(int s, const char* string)
{
  ssize_t result;

  assert(string);

  result = send(s, string, strlen(string), 0);
  if (result != (ssize_t)strlen(string)) {
    die("unexpected failure writing a string to the GTH");
  }
}

static void api_write(GTH_api *api, const char* command)
{
  const char *CT = "Content-type: text/xml\r\n";
  char CL[GTH_HEADER_BUFFER_LEN];
  int len;

  assert(api);
  assert(command);

  len = strlen(command);

  snprintf(CL, GTH_HEADER_BUFFER_LEN, "Content-length: %d\r\n\r\n", len);

  if (api->print_cmds) {
    fprintf(stderr, "GTH command: %s\n", command);
  }
  string_write(api->fd, CT);
  string_write(api->fd, CL);
  string_write(api->fd, command);
}

static void api_write_non_xml(int s,
			      const char* type,
			      const char* data,
			      int len)
{
  char CL[GTH_HEADER_BUFFER_LEN];
  int result;

  assert(type);
  assert(data);

  snprintf(CL, GTH_HEADER_BUFFER_LEN, "Content-length: %d\r\n\r\n", len);

  string_write(s, "Content-type: ");
  string_write(s, type);
  string_write(s, "\r\n");
  string_write(s, CL);
  result = send(s, data, len, 0);
  assert(result == len);
}

const char *gth_attribute_value(const GTH_resp *resp, const char *key) {
  int x;

  assert(resp);
  assert(key);

  for (x = 0; x < resp->n_attributes; x++) {
    if (strcmp(resp->attributes[x].key, key) == 0) {
      return resp->attributes[x].value;
    }
  }

  return 0;
}

char *gth_attribute_value_and_clear(GTH_resp *resp, const char *key) {
  int x;
  char *copy;

  assert(resp);
  assert(key);

  for (x = 0; x < resp->n_attributes; x++) {
    if (strcmp(resp->attributes[x].key, key) == 0) {
      copy = resp->attributes[x].value;
      resp->attributes[x].value = 0;
      return copy;
    }
  }

  return 0;
}


int gth_query_resource_attribute(GTH_api *api,
				 const char *name,
				 const char *key,
				 char *result,
				 int max_result) {
  char buffer[MAX_COMMAND];
  GTH_resp *resp;
  GTH_resp *resource;
  int x;
  int retval = -6;

  assert(api);
  assert(name);
  assert(key);
  assert(result);

  assert(max_result > 1);
  *result = 0;

  snprintf(buffer, MAX_COMMAND, "<query><resource name='%s'/></query>", name);
  api_write(api, buffer);

  resp = gth_next_non_event(api);

  if (resp == 0) return -9;

  if (resp->type == GTH_RESP_STATE
      && resp->n_children == 1
      && resp->children[0].type == GTH_RESP_RESOURCE)
    {
      resource = resp->children;

      for (x = 0; x < resource->n_children; x++) {
	GTH_resp *attribute;
	const char *name;
	const char *value;

	attribute = resource->children+x;

	assert(attribute->type == GTH_RESP_ATTRIBUTE);

	name  = gth_attribute_value(attribute, "name");
	value = gth_attribute_value(attribute, "value");

	assert(name);
	assert(value);

	if (strcmp(name, key) == 0) {
	  strncpy_s(result, max_result, value, max_result - 1);
	  result[max_result - 1] = 0;
	  retval = 0;
	}
      }
    }
  else
    {
      retval = -1;
    }

  gth_free_resp(resp);

  return retval;
}

int gth_query_job(GTH_api *api,
                  const char *id,
                  char *owner,
                  GTH_attribute **attributes,
                  int *n_attributes)
{
  char buffer[MAX_COMMAND];
  GTH_resp *resp;
  GTH_resp *job;
  const char *owner_copy;
  int x;
  int retval = 0;

  assert(api);
  assert(id);
  assert(attributes);
  assert(n_attributes);

  snprintf(buffer, MAX_COMMAND, "<query><job id='%s'/></query>", id);
  api_write(api, buffer);
  resp = gth_next_non_event(api);

  if (resp == 0) return -9;

  if (resp->type == GTH_RESP_STATE
      && resp->n_children == 1)
    {
      job = resp->children;

      owner_copy = gth_attribute_value(job, "owner");
      strncpy_s(owner, MAX_JOB_ID, owner_copy, MAX_JOB_ID - 1);

      *n_attributes = job->n_children;
      *attributes = checked_malloc(sizeof(GTH_attribute) * *n_attributes);

      for (x = 0; x < job->n_children; x++) {
	GTH_resp *attribute;
	char *name;
	char *value;

	attribute = job->children+x;
	if (attribute->type != GTH_RESP_ATTRIBUTE)
	  die("invalid response from GTH");

	name  = gth_attribute_value_and_clear(attribute, "name");
	value = gth_attribute_value_and_clear(attribute, "value");

	(*attributes)[x].key   = name;
	(*attributes)[x].value = value;
      }
    }
  else
    {
      retval = -1;
    }

  gth_free_resp(resp);
  return retval;
}

static int is_text_following_resource_query(const char *name)
{
  assert(name);

  return (!strcmp(name, "system_log")
	  || !strcmp(name, "system_log_recent")
	  || !strcmp(name, "start_script")
	  || !strcmp(name, "application_log")
	  || !strcmp(name, "application_log_recent")
	  || !strcmp(name, "standby_application_log")
	  || !strcmp(name, "standby_system_log")
	  );
}

static int query_single_resource(GTH_api *api,
				 const char *name,
				 GTH_attribute **attributes,
				 int *n_attributes)
{
  char buffer[MAX_COMMAND];
  GTH_resp *resp;
  GTH_resp *resource;
  int x;
  int retval = 0;

  assert(api);
  assert(name);
  assert(attributes);
  assert(n_attributes);

  snprintf(buffer, MAX_COMMAND, "<query><resource name='%s'/></query>", name);
  api_write(api, buffer);
  resp = gth_next_non_event(api);

  if (resp == 0) return -9;

  if (resp->type == GTH_RESP_STATE
      && resp->n_children == 1
      && resp->children[0].type == GTH_RESP_RESOURCE)
    {
      resource = resp->children;

      *n_attributes = resource->n_children;
      *attributes = 0;

      // Some queries return a text/plain section after the XML
      if (is_text_following_resource_query(name))
	{
	  int result;
	  char *text_buffer = checked_malloc(MAX_LOGFILE);

	  result = next_api_response(api, text_buffer, MAX_LOGFILE);
	  if (result != 0)
	    {
	      free(text_buffer);
	      gth_free_resp(resp);
	      return result;
	    }

	  (*n_attributes)++;
	  *attributes = checked_malloc(sizeof(GTH_attribute) * *n_attributes);
	  (*attributes)[*n_attributes - 1].key = "log_body";
	  (*attributes)[*n_attributes - 1].value = text_buffer;
	}
      else
	{
	  *attributes = checked_malloc(sizeof(GTH_attribute) * *n_attributes);
	}

      for (x = 0; x < resource->n_children; x++) {
	GTH_resp *attribute;
	char *name;
	char *value;

	attribute = resource->children+x;
	if (attribute->type != GTH_RESP_ATTRIBUTE)
	  die("invalid response from GTH");

	name  = gth_attribute_value_and_clear(attribute, "name");
	value = gth_attribute_value_and_clear(attribute, "value");

	(*attributes)[x].key   = name;
	(*attributes)[x].value = value;
      }
    }
  else if (resp->type == GTH_RESP_STATE
           && !strcmp(name, "schedule")
           && resp->children[0].type == GTH_RESP_JOB)
    {
      *n_attributes = resp->n_children;
      *attributes = checked_malloc(sizeof(GTH_attribute) * *n_attributes);

      for (x = 0; x < resp->n_children; x++) {
        GTH_resp *job;
        char *id;
        char *owner;

        job = resp->children + x;
        if (job->type != GTH_RESP_JOB)
          die("invalid response from GTH");

        id = gth_attribute_value_and_clear(job, "id");
	owner = gth_attribute_value_and_clear(job, "owner");

	(*attributes)[x].key   = id;
	(*attributes)[x].value = owner;

      }
    }
  else
    {
      retval = -1;
    }

  gth_free_resp(resp);
  return retval;
}

static int query_inventory(GTH_api *api,
			   GTH_attribute **attributes,
			   int *n_attributes)
{
  GTH_resp *resp;
  GTH_resp *resource;
  int x;
  int retval = 0;

  assert(api);
  assert(attributes);
  assert(n_attributes);

  api_write(api, "<query><resource name='inventory'/></query>");
  resp = gth_next_non_event(api);

  if (resp == 0) return -9;

  if (resp->type == GTH_RESP_STATE)
    {
      *attributes = checked_malloc(sizeof(GTH_attribute) * resp->n_children);
      *n_attributes = resp->n_children;

      for (x = 0; x < resp->n_children; x++) {
	char *name;

	resource = resp->children+x;
	if (resource->type != GTH_RESP_RESOURCE)
	  die("invalid response from GTH");

	name  = gth_attribute_value_and_clear(resource, "name");

	(*attributes)[x].key   = name;
	(*attributes)[x].value = 0;
      }
    }
  else
    {
      retval = -1;
    }

  gth_free_resp(resp);

  return retval;
}

int gth_query_resource(GTH_api *api,
		       const char *name,
		       GTH_attribute **attributes,
		       int *n_attributes)
{
  if (!strcmp(name, "inventory"))
    return query_inventory(api, attributes, n_attributes);
  else
    return query_single_resource(api, name, attributes, n_attributes);
}


int gth_set(GTH_api *api,
	    const char *resource,
	    const GTH_attribute *attributes,
	    int n_attributes)
{
  return gth_enable_or_set("set", api, resource, attributes, n_attributes);
}

// Special case of set for just one attribute.
int gth_set_single(GTH_api *api,
		   const char *resource,
		   const char *attribute,
		   const char *value)
{
  const GTH_attribute a = {(char*)attribute, (char*)value};
  return gth_set(api, resource, &a, 1);
}

int gth_reset(GTH_api *api, const char *resource)
{
  return single_arg_ok_response(api,
				"<reset><resource name='%s'/></reset>",
				resource);
}

GTH_resp *gth_raw_xml(GTH_api *api, const char* string)
{
  GTH_resp *resp;

  assert(api);
  assert(string);

  api_write(api, string);
  resp = gth_next_non_event(api);

  return resp;
}


// Figure out this machine's IP address. We ask the GTH, that way
// the answer is correct even on multihomed machines.
static void my_ip_address(GTH_api *api)
{
  GTH_resp *resp;
  char buffer[1000];
  const char *job_id;
  const char *ip_addr;

  assert(api);

  // gth2_failsafe_9 has a bug which prevents us from querying 'self' in
  // failsafe mode. So we check that we're not in failsafe mode first by
  // sending a nop. We'll get a failsafe event straight away.
  // (Actually, we get the failsafe event without a <nop>. The <nop> is
  // needed so that we get an answer even in 'system' mode).

  gth_nop(api);

  if (api->is_failsafe) {
    api->my_ip[0] = 0;
    return;
  }

  api_write(api, "<query><job id='self'/></query>");

  resp = gth_next_non_event(api);

  assert(resp && resp->type == GTH_RESP_STATE);
  assert(resp->n_children == 1 && resp->children[0].type == GTH_RESP_JOB);
  job_id = gth_attribute_value(resp->children+0, "id");
  snprintf(buffer, sizeof(buffer), "<query><job id='%s'/></query>", job_id);

  gth_free_resp(resp);
  api_write(api, buffer);

  resp = gth_next_non_event(api);
  assert(resp && resp->type == GTH_RESP_STATE);
  assert(resp->n_children == 1
	 && resp->children[0].type == GTH_RESP_CONTROLLER);
  ip_addr = gth_attribute_value(resp->children+0, "ip_addr");
  assert(strlen(ip_addr) < sizeof(api->my_ip));

  strncpy_s(api->my_ip, sizeof(api->my_ip), ip_addr, sizeof(api->my_ip) - 1);
  gth_free_resp(resp);
}

const char *gth_my_ip_address(GTH_api *api)
{
  assert(api);
  return api->my_ip;
}

void gth_free_attributes(GTH_attribute *attributes, int n_attributes)
{
  int x;

  assert(attributes);

  for (x = 0; x < n_attributes; x++) {
    free(attributes[x].key);
    free(attributes[x].value);
  }

  free(attributes);
}

// Windows has its own version of sleep(), which is called Sleep() and
// it sleeps for milliseconds instead of seconds. So we wrap it.
void sleep_seconds(int seconds) {
  #ifdef WIN32
  Sleep(seconds * 1000);
  #else
  sleep(seconds);
  #endif
}

int gth_wait_for_reboot(const char *hostname) {
  int max_seconds_to_wait = 60;
  GTH_api api;
  int result;

  assert(hostname);

  sleep_seconds(10);

  while (max_seconds_to_wait--) {
    result = gth_connect(&api, hostname, 0);
    if (result == 0) {
      gth_bye(&api);
      return 0;
    }
    sleep_seconds(1);
  }

  return -1;
}

int gth_wait_for_event(GTH_api *api, const int milliseconds)
{
  struct timeval timeout = {milliseconds / 1000,
                            (milliseconds % 1000) * 1000};
  fd_set readfds;
  int result;

  FD_ZERO(&readfds);
  FD_SET(api->fd, &readfds);

  result = select(api->fd + 1, &readfds, 0, 0, &timeout);

  if (result == 0) return -1; // timeout

  return 0;
}

int gth_process_event(GTH_api *api)
{
  int result;
  char buffer[MAX_COMMAND];
  GTH_resp *resp = 0;

  result = next_api_response(api, buffer, sizeof(buffer));
  if (result != 0) {
    return 1;
  }
  resp = gth_parse(buffer);

  assert(resp);
  assert(resp->type == GTH_RESP_EVENT);
  assert(api->event_handler);
  (*(api->event_handler))(api, resp);
  gth_free_resp(resp);

  return 0;
}


#define MAX_QUERY_LENGTH 1000

void gth_switch_to(const char *hostname,
		   const char *system_name,
		   const int verbose)
{
  char response[MAX_QUERY_LENGTH];
  char imagename[50];
  GTH_api api;
  int result;

  assert(hostname);
  assert(system_name);

  result = gth_connect(&api, hostname, 0);
  if (result != 0) {
    die("unable to connect to the specified GTH");
  }

  if (verbose) {
    fprintf(stderr, "switching to %s\n", system_name);
  }

  snprintf(imagename, sizeof(imagename), "%s_image", system_name);

  result = gth_query_resource_attribute(&api, imagename,
					"busy", response, MAX_QUERY_LENGTH);

  if (strcmp(response, "false") == 0) {
    if (strcmp(system_name, "system") == 0) {
      gth_set_single(&api, "os", "boot mode", "normal");
    } else {
      gth_set_single(&api, "os", "boot mode", "failsafe");
    }

    gth_reset(&api, "cpu");

    if (verbose) {
      fprintf(stderr, "waiting for the GTH to reboot\n");
    }

    gth_wait_for_reboot(hostname);

    if (verbose) {
      fprintf(stderr, "GTH back up\n");
    }
  } else {
    assert(strcmp(response, "true") == 0);
    gth_bye(&api);
  }

  result = gth_connect(&api, hostname, 0);
  if (result != 0) {
    die("Unable to (re)connect to the GTH");
  }
  gth_query_resource_attribute(&api, imagename, "busy", response,
			       MAX_QUERY_LENGTH);

  if (strcmp(response, "true")) {
    die("failed to switch images\n");
  }
  gth_bye(&api);
}

int gth_map(GTH_api *api,
	    const char *resource,
	    char *name,
	    int max_name)
{
  char buffer[MAX_COMMAND];
  GTH_resp *resp;
  int retval = 0;

  assert(api);
  assert(name);

  assert(max_name > 1);
  name[0] = 0;

  snprintf(buffer, MAX_COMMAND, "<map target_type='pcm_source'>"
	   "<sdh_source name='%s'/></map>", resource);
  api_write(api, buffer);

  resp = gth_next_non_event(api);

  if (resp == 0) return -9;

  if (resp->type == GTH_RESP_RESOURCE)
    {
      strncpy_s(name, max_name, resp->attributes[0].value, max_name - 1);
    }
  else
    {
      retval = -1;
    }

  gth_free_resp(resp);

  return retval;
}

int gth_unmap(GTH_api *api,
	      const char *resource)
{
  return single_arg_ok_response(api, "<unmap name='%s'/>", resource);
}

static int new_atm_aal_monitor(GTH_api *api,
                               const int tag,
                               const char *span,
                               const int timeslots[],
                               const int n_timeslots,
                               const int vpi,
                               const int vci,
                               char *job_id,
                               const char *ip,
                               const int port,
                               const int aal)
{
  char command[MAX_COMMAND];
  char sources[MAX_COMMAND];
  int result;
  const char* template;

  assert(n_timeslots < 32 && n_timeslots > 0);

  template = "<new><atm_aal%d_monitor ip_addr='%s' ip_port='%d' tag='%d'"
    "vpi='%d' vci='%d'>"
    "%s</atm_aal%d_monitor></new>";

  format_sources(span, timeslots, n_timeslots, 64, sources);

  result = snprintf(command, MAX_COMMAND, template,
		    aal, ip, port, tag, vpi, vci, sources, aal);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  return result;
}

static int new_sdh_atm_aal_monitor(GTH_api *api,
                                   const int tag,
                                   const char *source,
                                   const int vpi,
                                   const int vci,
                                   char *job_id,
                                   const char *ip,
                                   const int port,
                                   const int aal)
{
  char command[MAX_COMMAND];
  int result;
  const char* template;

  template = "<new><atm_aal%d_monitor ip_addr='%s' ip_port='%d' tag='%d'"
    "vpi='%d' vci='%d'>"
    "<sdh_source name='%s'/></atm_aal%d_monitor></new>";

  result = snprintf(command, MAX_COMMAND, template,
		    aal, ip, port, tag, vpi, vci, source, aal);
  assert(result < MAX_COMMAND);
  api_write(api, command);
  result = recv_job_id(api, job_id);

  return result;
}



// Win32 requires an initialisation call to its socket library at program
// startup.
void win32_specific_startup() {
#ifdef WIN32
  WSADATA wsa_data;

  int result;

  result = WSAStartup(0x0202, &wsa_data);
  if (result != 0) {
    die("WSAStartup failed. Does this OS have working TCP/IP?");
  }
#endif

  return;
}

#ifndef WIN32
// Microsoft use a "more secure" variant of fopen(). So, if we're using
// gcc for a non-windows target, provide a workalike:
int fopen_s(FILE **file, const char *filename, const char *mode)
{
  *file = fopen(filename, mode);
  if (!file)
    return errno;
  else
    return 0;
}
#endif
