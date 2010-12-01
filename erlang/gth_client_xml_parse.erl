%%----------------------------------------------------------------------
%% Parser for XML responses from the GTH API. This parses all possible
%% responses from the GTH.
%%
%% Used by gth.erl. Some customers also use it directly.
%%
%% Author: Matthias Lang (matthias@corelatus.com)
%%
%% Copyright (c) 2009, Corelatus AB Stockholm
%%
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of Corelatus nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY Corelatus ''AS IS'' AND ANY
%% EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL Corelatus BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%% 

%%
%%----------------------------------------------------------------------
-module(gth_client_xml_parse).
-export([string/1]).
-include("gth_api.hrl").

%% for debugging
-export([step/0, trace/0, tracer/0]).

%%----------------------------------------------------------------------
%% Normal entry point.
%% S: string()
%% Return: {ok, #resp_tuple} | {error, Reason}
string(S) ->
    case catch (do_string(S)) of
	{ok, Cmd} ->
	    {ok, Cmd};

	{'EXIT', Reason} ->
	    error_logger:error_report(xml_command, {exit, Reason}),
	    error_logger:error_report(xml_command, {bad_reply, S}),
	    {error, parse};
	
	X ->
	    {error, X}
    end.

%% Returns {ok, #resp_tuple} | {error, Reason}
do_string(S) ->
    {[Tree], []} = gth_xml_scan:scan_and_parse(S),
    checked(Tree).

%%======================================================================
%% Check the parse tree. Not an exhaustive check, but useful anyway.
%%
%% All the mapping from plain tuples to records is for backwards
%% compatibility with earlier versions of this parser. A customer who
%% wanted to eliminate that step, most easily by using
%% gth_xml_scan:scan_and_parse(), i.e. no need for this module at all.
%% 

checked({N, A, C, T})
  when N == error;
       N == job;
       N == ok ->
    [] = C,
    {ok, #resp_tuple{name=N, attributes=A, clippings=T}};

checked({event, [], C, T}) ->  
    Map = lists:map(fun event_child/1, C),
    {ok, #resp_tuple{name=event, children = Map, clippings=T}};

checked({state, [], C, T}) ->  
    Map = lists:map(fun state_child/1, C),
    {ok, #resp_tuple{name=state, children=Map, clippings=T}};

checked(_) -> 
    {error, unknown_tle}.

event_child({Name, A, [], T}) 
  when Name == alarm; 
       Name == alert; 
       Name == atm_message; 
       Name == ebs; 
       Name == fatality; 
       Name == fault; 
       Name == f_relay_message; 
       Name == info; 
       Name == l1_message; 
       Name == l2_alarm; 
       Name == l2_socket_alert; 
       Name == lapd_message; 
       Name == level; 
       Name == message_ended; 
       Name == mtp2_message; 
       Name == slip; 
       Name == sync_message; 
       Name == tone ->
    #resp_tuple{name=Name, attributes=A, children=[], clippings=T};

event_child({backup, [], C, T}) ->
    F = fun({job, A, [], []}) -> #resp_tuple{name=job, attributes=A} end, 
    #resp_tuple{name=backup, children=lists:map(F, C), clippings=T}.

%% These state children have no children
state_child({Name, A, [], T})
  when Name == controller;
       Name == error;
       Name == job;
       Name == player ->
    #resp_tuple{name=Name, attributes=A, clippings=T};

%% These state children have (only) attributes  as their children
state_child({Name, A, C, T})
  when Name == resource;
       Name == atm_aal0_monitor;
       Name == atm_aal2_monitor;
       Name == atm_aal5_monitor;
       Name == ss5_linesig_monitor;
       Name == ss5_registersig_monitor;
       Name == cas_r2_linesig_monitor;
       Name == cas_r2_mfc_detector;
       Name == f_relay_monitor;
       Name == lapd_monitor;
       Name == mtp2_monitor ->
    Map = lists:map(fun attribute/1, C),
    #resp_tuple{name=Name, attributes=A, children=Map, clippings=T};

state_child({ebs, A, C, T}) ->
    Map = lists:map(fun module/1, C),
    #resp_tuple{name=ebs, attributes=A, children=Map, clippings=T}.

attribute({attribute, A, [], []}) ->
    #resp_tuple{name=attribute, attributes=A}.

module({module, A, [], []}) ->
    #resp_tuple{name=module, attributes=A}.

%%----------------------------------------------------------------------
%% For debugging.

trace() ->
    %% Trace everything
    erlang:trace_pattern({?MODULE, '_', '_'}, true, [local]),

    %% Except, don't trace the scanner and the tracer
    erlang:trace_pattern({?MODULE, 'tracer', '_'}, false, [local]),

    Pid = spawn(?MODULE, tracer, []),
    erlang:trace(all, true, [call, return_to, {tracer, Pid}]).

tracer() ->
    receive
	X ->
	     io:fwrite("~p\n", [X])
    end,
    tracer().

%% Single stepping. On the parser by default.
step() ->
    debugger:start(),
    int:i(xml_parse, [{i, "../include"}]),
    int:break_in(?MODULE, parse, 1).
