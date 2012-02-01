%%----------------------------------------------------------------------
%% Title: record definitions for the GTH API. 
%% Author: Matthias (matthias@corelatus.se)
%%
%% Purpose: All GTH-related records which are passed between API modules are
%%          here, including the ones used by gth_client_xml_parse.erl
%% 

%% Responses from the GTH. A general XML tuple.
-record(resp_tuple, {
	  name            :: atom(),             % e.g. 'ok' | 'error' | ...
	  attributes = [] :: [{Key::string(), Value::string()}],
	  children   = [] :: [#resp_tuple{}],
	  clippings  = [] :: [string()]
	}).

