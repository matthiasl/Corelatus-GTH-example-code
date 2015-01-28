%%----------------------------------------------------------------------
%% Scanner for XML commands as used by GTH.
%%
%% Optionally turns the scanned tokens into a parse tree.
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
%%----------------------------------------------------------------------
-module(gth_xml_scan).
-export([scan/1, scan_and_parse/1]).
-export([xml_tags/0]).

scan([$<|T])  -> [open|in_tag(T)];
scan([])      -> [];
scan(L) ->
    End = string:cspan(L, "<"),
    S = string:substr(L, 1, End),
    case strip(S) of
	[] ->            scan(string:substr(L, End+1));
	_ ->  [{text, S}|scan(string:substr(L, End+1))]
    end.

%% Return: { [Parse_tree], [token()]}
%% Parse_tree = {Name, Attributes, Children, Text}
%%
%% exits on failure (parse error).
%%
scan_and_parse(String) ->
    Tokens = scan(String),
    parse_trees(Tokens).

%%----------------------------------------------------------------------
%% Scanner internals
in_tag([])      -> [];
in_tag([$ |T])  -> whitespace(T);
in_tag([$\n|T]) -> whitespace(T);
in_tag([$\r|T]) -> whitespace(T);
in_tag([$\t|T]) -> whitespace(T);
in_tag([$\"|T])  -> instring(T, $\");
in_tag([$\'|T])  -> instring(T, $\');

in_tag([$>|T])  -> [close|scan(T)];
in_tag([$=|T])  -> [equal|in_tag(T)];
in_tag([$/|T])  -> [slash|in_tag(T)];
in_tag(L)       -> name(L).

%% Discard leading whitespace
strip(" " ++ T) -> strip(T);
strip("\r" ++ T) -> strip(T);
strip("\n" ++ T) -> strip(T);
strip("\t" ++ T) -> strip(T);
strip(X) -> X.

%% Discard all whitespace
whitespace(L) ->
    End = string:span(L, " \n\r\t"),
    in_tag(string:substr(L, End+1)).

%% Strings can be either single or double quoted. Ref: XML spec "AttValue"
instring(T, Quote) ->
    End = string:chr(T, Quote),
    S = string:substr(T, 1, End-1),
    [{string, S}|in_tag(string:substr(T, End+1))].

name(T) ->
    End = string:cspan(T, " =\r\n/><"),
    case End of
	0 -> exit("zero-length name");
	_ -> ok
    end,
    S = string:substr(T, 1, End),
    [{name, S}|in_tag(string:substr(T, End+1))].

%%-----------------------------------------------------------------------
%% Parser internals
%%

%% Trick: the parser uses 'list_to_existing_atom()' to prevent new
%% atoms from being created by malicious input to the parser. This
%% function exists to make sure all the legal atoms already exist. You
%% don't need to call it, ever.
%%
%% One way to generate the list is
%%
%%  grep ELEMENT gth_out.dtd | cut -f 2 -d ' ' | sort | uniq
%%
xml_tags() ->
    [error, job, ok,

     event, alarm, alert, atm_message, backup, fatality, fault,
     f_relay_message, info, l1_message, sdh_message, l2_alarm, l2_socket_alert,
     lapd_message, level, message_ended, mtp2_message, slip,
     sync_message, ebs, tone, module, attribute,

     state,

     atm_aal0_layer, atm_aal0_monitor, atm_aal2_monitor, atm_aal5_monitor,
     cas_r2_linesig_monitor, cas_r2_mfc_detector, cas_r2_linesig_transmitter,
     controller,
     fr_layer, fr_monitor, lapd_layer, lapd_monitor, level_detector,
     mtp2_monitor, player, raw_monitor, resource, ss5_linesig_monitor,
     ss5_registersig_monitor, tone_detector,

     pcm_sink
    ].

parse_trees([]) ->
    {[], []};
parse_trees(Tokens = [open, slash|_]) ->
    {[], Tokens};
parse_trees(Tokens) ->
    {Tree, Tokens_after_tree} = parse_tree(Tokens),
    {Trees, Tokens_after_trees} = parse_trees(Tokens_after_tree),
    {[Tree|Trees], Tokens_after_trees}.

%% Return: {Tree, Leftover_tokens}
parse_tree([open, {name, Name}|Tokens]) ->
    {Attributes, Tokens_after_attributes} = attributes(Tokens),
    Atomic = list_to_existing_atom(Name),

    case Tokens_after_attributes of
	[slash, close|Leftover] ->
	    Tree = {Atomic, Attributes, [], []},
	    { Tree, Leftover };

	[close|Tokens_after_tag] ->
	    {Text, Tokens_after_text} = parse_inside_tag(Tokens_after_tag),
	    {Children, Tokens_after_children} = parse_trees(Tokens_after_text),
	    [open, slash, {name, Name}, close|Leftover] = Tokens_after_children,
	    Tree = {Atomic, Attributes, Children, Text},
	    {Tree, Leftover}
    end;

parse_tree(Tokens = [open, slash|_]) ->
    {[], Tokens}.

parse_inside_tag([{text, T}|Rest]) ->
    {T, Rest};
parse_inside_tag(X) ->
    {"", X}.

%% Extracts the attributes from within a tag
%% Return { [{Name, Value}], Leftover}
attributes(T = [close|_]) ->
    {[], T};
attributes(T = [slash,close|_]) ->
    {[], T};
attributes([{name, Name}, equal, {string, Value}|T]) ->
    {More, LO} = attributes(T),
    {[{Name, Value}|More], LO}.
