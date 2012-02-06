%%----------------------------------------------------------------------
%% Title: Record which represents an XML tree
%% Author: Matthias (matthias@corelatus.se)
%%
%% For the GTH's purposes, any XML can be represented as a tree, with
%% each node in the tree having:
%%
%%   - a name
%%   - zero or more attributes
%%   - zero or more children (each of which is an XML tree)
%%   - possibly, some text ("clippings")
%%
%% Example:
%%
%%    <a><b p='q'/></a>
%%
%%   is equivalent to
%% 
%%    #resp_tuple{name=a, attributes=[], clippings=[],
%%                children=[#resp_tuple{name=b, attributes=[{"p", "q"}]}]}
%%
%% Whenever the GTH parses XML, it returns a #resp_tuple.

-record(resp_tuple, {
	  name            :: atom(),             % e.g. 'ok' | 'error' | ...
	  attributes = [] :: [{Key::string(), Value::string()}],
	  children   = [] :: [#resp_tuple{}],
	  clippings  = [] :: [string()]
	}).

