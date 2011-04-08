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
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <assert.h>

#ifdef WIN32
#include <winsock2.h>
typedef int socklen_t;
#else
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#endif // WIN32

#include "gth_win32_compat.h"
#include "gth_apilib.h"
#include "gth_client_xml_parse.h"

#define MAX_COMMAND 10000
#define MAX_RESPONSE 10000
#define MAX_LOGFILE 20000000

const int GTH_API_PORT = 2089;        // TCP port a Corelatus GTH listens on

//----------------------------------------------------------------------
// Forward declarations.

static int read_header_line(int fd, char *line);
static int definite_read(int fd,
			 int length,
			 char *response,
			 const int max_response_length);
static GTH_resp *gth_next_non_event(GTH_api *api);
static void string_write(int s, const char* string);
static char *attribute_value(const GTH_resp *resp, const char *key);
static char *attribute_value_and_clear(GTH_resp *resp, const char *key);
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

// Read the next GTH response from the given API connection.
//
// The response is written to the caller-provided response buffer,
// truncated to response_length, including the zero termination.
//
// Return: 0 on success
//         GTH_TRUNCATED if the output was larger than the provided buffer
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
  exit(-1);
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
    const char *reason = attribute_value(child, "reason");

    if (!strcmp(reason, "failsafe_mode")) {
      api->is_failsafe = 1;
    } else {
      printf("Ignoring an <info> with reason=%s\n", reason);
    }
    break;
  }

  case GTH_RESP_L1_MESSAGE: {
    gth_print_tree(resp);    
    break;
  }

  case GTH_RESP_SYNC_MESSAGE: {
    gth_print_tree(resp);    
    break;
  }


  default:
    printf("gth_event_handler got an event, handling with default handler\n");
    gth_print_tree(resp);    
    break;
  }

  // do not free the resp, it's not yours to free. (handlers may be chained)
}


int gth_connect(GTH_api *api, const char *address) 
{
  struct sockaddr_in gth_addr;
  struct hostent* host;

  assert(api);
  api->is_failsafe = 0;
  api->print_cmds = 0;
  api->event_handler = &gth_event_handler;

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

int gth_bye(GTH_api *api) 
{
  api_write(api, "<bye/>");

  if (check_api_response(api, GTH_RESP_OK, 0))
    {
      return -1;
    }

  return 0;  
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

  result = getsockname(s, (struct sockaddr*)&addr, &addr_size);
  assert(result == 0);
  assert(addr_size == sizeof addr);

  *port = ntohs(addr.sin_port);
  return s;
}

int gth_wait_for_accept(int listen_socket) 
{
  int data_socket = -1;

  data_socket = accept(listen_socket, 0, 0);
  assert(data_socket >= 0);

  return data_socket;
}

// We're expecting an install-complete event, wait for it
static int gth_wait_for_install_complete(GTH_api *api)
{
  char buffer[MAX_COMMAND];
  int result;
  GTH_resp *resp = 0;

  assert(api);

  for (;;) {
    GTH_resp *child;

    result = next_api_response(api, buffer, sizeof(buffer));
    if (result != 0) {
      return 0;
    }
    resp = gth_parse(buffer); 
  
    assert(resp);
    assert(resp->type == GTH_RESP_EVENT);
    assert(resp->n_children == 1);

    child = resp->children + 0;
  
    if (child->type == GTH_RESP_INFO) {
      if (!strcmp("install_done", attribute_value(child, "reason"))) {
	gth_free_resp(resp);
	return 0;
      }
      else {
	gth_print_tree(resp);
	gth_free_resp(resp);
	return -1;
      }
    }
    
    assert(*api->event_handler);
    (*api->event_handler)(api, resp);
  }

  return 0;  
}

int gth_install(GTH_api *api,
		const char *name,
		const char *type,
		const char *data,
		const int length)
{
  char buffer[MAX_COMMAND];
  GTH_resp *resp;

  assert(api);

  snprintf(buffer, MAX_COMMAND, "<install name='%s'/>", name);
  api_write(api, buffer);
  api_write_non_xml(api->fd, type, data, length);
  
  if (check_api_response(api, GTH_RESP_OK, &resp))
    {
      if (resp) 
	{
	  gth_print_tree(resp);
	  gth_free_resp(resp);
	}
      return -1;
    }

  // Upgrades send an <info> when they're really complete.
  if (!strcmp(type, "binary/filesystem")) {
    gth_wait_for_install_complete(api);
  }

  return 0;
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


int gth_new_mtp2_monitor(GTH_api *api,
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

  template = "<new><mtp2_monitor ip_addr='%s' ip_port='%d' tag='%d'>"
    "<pcm_source span='%s' timeslot='%d'/>"
    "</mtp2_monitor></new>";

  result = snprintf(command, MAX_COMMAND, template, ip, port, tag, span, ts);
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

// We're expecting a <message_ended> event, wait for it
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
      event_job_id = attribute_value(child, "id");
      if (!strcmp(event_job_id, job_id)) {
	gth_free_resp(resp);
	return 0;
      }
    }
    
    assert(*api->event_handler);
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

  assert(api);
  assert(id);

  resp = gth_next_non_event(api);
  if (resp == 0) return -1;
  
  if (resp->type != GTH_RESP_JOB) return -2;
  
  id_attr = attribute_value(resp, "id");
  assert(id_attr && strlen(id_attr) < MAX_JOB_ID);

  strcpy(id, id_attr);

  gth_free_resp(resp);

  return 0;
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
  int result;

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

  result1 = sscanf(content_type, "Content-type: %s", type);
  result2 = sscanf(content_length, "Content-length: %d", &length);

  if (result1 != 1 || result2 != 1) {
    closesocket(api->fd);
    api->fd = -1;
    return -2;
  }

  if (strcmp(type, "text/xml") != 0 
      && strcmp(type, "text/plain") != 0) {
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

    assert(*api->event_handler);
    (*(api->event_handler))(api, resp);
    gth_free_resp(resp);
  }
}

static void string_write(int s, const char* string) 
{
  size_t result;

  assert(string);
  
  result = send(s, string, strlen(string), 0);
  if (result != strlen(string)) {
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

// Returns null if the attribute isn't there.
//
// Does not change the resp at all.
static char *attribute_value(const GTH_resp *resp, const char *key) {
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

// Returns null if the attribute isn't there.
//
// Clears the value pointer in the resp, the caller is responsible for
// calling free() on the returned value.
static char *attribute_value_and_clear(GTH_resp *resp, const char *key) {
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
  
  if (!resp) return -9;
  if (resp->type != GTH_RESP_STATE) return -1;
  if (resp->n_children != 1) return -2;
  if (resp->children[0].type != GTH_RESP_RESOURCE) return -3;

  resource = resp->children;

  for (x = 0; x < resource->n_children; x++) {
    GTH_resp *attribute;
    const char *name;
    const char *value;

    attribute = resource->children+x;

    if (attribute->type != GTH_RESP_ATTRIBUTE) return -4;

    name  = attribute_value(attribute, "name");
    value = attribute_value(attribute, "value");

    if (!name || !value) return -5;

    if (strcmp(name, key) == 0) {
      strncpy(result, value, max_result);
      result[max_result - 1] = 0;
      retval = 0;
    }
  }
 
  gth_free_resp(resp);

  return retval;
}

static int is_text_following_resource_query(const char *name)
{
  assert(name);

  return (!strcmp(name, "system_log")
	  || !strcmp(name, "system_log_recent")
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

  assert(api);
  assert(name);
  assert(attributes);
  assert(n_attributes);

  snprintf(buffer, MAX_COMMAND, "<query><resource name='%s'/></query>", name);
  api_write(api, buffer);
  resp = gth_next_non_event(api);

  if (!resp) return -9;
  if (resp->type != GTH_RESP_STATE) return -1;
  if (resp->n_children != 1) return -2;
  if (resp->children[0].type != GTH_RESP_RESOURCE) return -3;

  resource = resp->children;

  *n_attributes = resource->n_children;
  *attributes = 0;

  // Some queries return a text/plain section after the XML
  if (is_text_following_resource_query(name))
    {
      int result;
      char *text_buffer = malloc(MAX_LOGFILE);

      assert(text_buffer);

      result = next_api_response(api, text_buffer, MAX_LOGFILE);
      if (result)
	{
	  return result;
	}

      (*n_attributes)++;
      *attributes = malloc(sizeof(GTH_attribute) * *n_attributes);
      assert(*attributes);      
      (*attributes)[*n_attributes - 1].key = "log_body";
      (*attributes)[*n_attributes - 1].value = text_buffer;
    }
  else
    {
      *attributes = malloc(sizeof(GTH_attribute) * *n_attributes);
      assert(*attributes);      
    }

  for (x = 0; x < resource->n_children; x++) {
    GTH_resp *attribute;
    char *name;
    char *value;

    attribute = resource->children+x;
    if (attribute->type != GTH_RESP_ATTRIBUTE) 
      die("invalid response from GTH");

    name  = attribute_value_and_clear(attribute, "name");
    value = attribute_value_and_clear(attribute, "value");

    (*attributes)[x].key   = name;
    (*attributes)[x].value = value;
  }
 
  gth_free_resp(resp);

  return 0;
}

static int query_inventory(GTH_api *api, 
			   GTH_attribute **attributes, 
			   int *n_attributes) 
{
  GTH_resp *resp;
  GTH_resp *resource;
  int x;

  assert(api);
  assert(attributes);
  assert(n_attributes);

  api_write(api, "<query><resource name='inventory'/></query>");
  resp = gth_next_non_event(api);
  
  if (!resp) return -9;
  if (resp->type != GTH_RESP_STATE) return -1;
  *attributes = malloc(sizeof(GTH_attribute) * resp->n_children);
  assert(*attributes);
  *n_attributes = resp->n_children;

  for (x = 0; x < resp->n_children; x++) {
    char *name;

    resource = resp->children+x;
    if (resource->type != GTH_RESP_RESOURCE) 
      die("invalid response from GTH");

    name  = attribute_value_and_clear(resource, "name");

    (*attributes)[x].key   = name;
    (*attributes)[x].value = 0;
  }
 
  gth_free_resp(resp);

  return 0;
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
  char buffer[MAX_COMMAND];
  int used;
  GTH_resp *resp;

  assert(api);
  assert(resource);
  assert(attributes);
  
  used = snprintf(buffer, MAX_COMMAND, "<set name='%s'>", resource);

  while (n_attributes > 0) {
    used += snprintf(buffer + used, MAX_COMMAND - used, 
		     "<attribute name='%s' value='%s'/>",
		     attributes->key, attributes->value);
    n_attributes--;
    attributes++;
  }

  used += snprintf(buffer + used, MAX_COMMAND - used, "</set>");
  
  api_write(api, buffer);

  if (check_api_response(api, GTH_RESP_OK, &resp))
    {
      if (resp)
	{
	  gth_print_tree(resp);
	  gth_free_resp(resp);
	}
      return -1;
    }
  
  return 0;
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
  assert(api);
  assert(resource);

  if (strcmp(resource, "cpu")) 
    return -1;

  api_write(api, "<reset><resource name='cpu'/></reset>");
  if (check_api_response(api, GTH_RESP_OK, 0))
    {
      return -1;
    }

  return 0;
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
  job_id = attribute_value(resp->children+0, "id");
  snprintf(buffer, sizeof(buffer), "<query><job id='%s'/></query>", job_id);

  gth_free_resp(resp);
  api_write(api, buffer);

  resp = gth_next_non_event(api);
  assert(resp && resp->type == GTH_RESP_STATE);
  assert(resp->n_children == 1 
	 && resp->children[0].type == GTH_RESP_CONTROLLER);
  ip_addr = attribute_value(resp->children+0, "ip_addr");
  assert(strlen(ip_addr) < sizeof(api->my_ip));

  strcpy(api->my_ip, ip_addr);
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
    result = gth_connect(&api, hostname);
    if (result == 0) {
      gth_bye(&api);
      return 0;
    }
    sleep_seconds(1);
  }

  return -1;  
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
  
  result = gth_connect(&api, hostname);

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

  result = gth_connect(&api, hostname);
  assert(result == 0);
  gth_query_resource_attribute(&api, imagename, "busy", response, 
			       MAX_QUERY_LENGTH);

  if (strcmp(response, "true")) {
    die("failed to switch images\n");
  }
  gth_bye(&api);
}

// Win32 requires an initialisation call to its socket library at program
// startup.
void win32_specific_startup() {
#ifdef WIN32
  WSADATA wsa_data;

  int result = WSAStartup(0x0202, &wsa_data);
  assert(result == 0);
#endif

  return;
}

#ifndef _MSC_VER
// Microsoft use a "more secure" variant of fopen(). So if we're not using
// a Microsoft compiler, provide a workalike:
int fopen_s(FILE **file, const char *filename, const char *mode)
{
  *file = fopen(filename, mode);
  if (!file)
    return errno;
  else 
    return 0;
}
#endif
