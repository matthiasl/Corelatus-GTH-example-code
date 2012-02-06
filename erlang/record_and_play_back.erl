%%-------------------------------------------------------------------
%% File    : record_and_play_back.erl
%% Author  : matthias <matthias@corelatus.se>
%%
%% Description : save a bit-exact copy of a timeslot to a file (record)
%%               replay the contents of a file to a timeslot (play)
%%
%%               This is a simple demo showing how to record and replay
%%               an E1 timeslot with a GTH. It's useful for debugging
%%               (e.g. you can be on-site and record a signalling link
%%               to a file and then take the file with you to analyse
%%               later).
%%
%%               The same API commands could be used to make a voicemail
%%               system.
%%
%% Typical session:
%%
%%    1> record_and_play_back:record("172.16.2.7", "1A", 1, "ts_dump.raw", 10).
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
%%-------------------------------------------------------------------
-module(record_and_play_back).
-export([record/5, play/4]).

%% Save the given timeslot to a file
record(GTH_IP, Span, Ts, Filename, Seconds) ->
    {ok, A} = gth:start_link(GTH_IP),
    setup_l1(A, Span),
    {ok, F} = file:open(Filename, [write, raw]),
    {ok, _Job, Data} = gth:new_recorder(A, Span, Ts),
    copy_to_file(Data, F, Seconds * 8000),
    ok = gth:bye(A).

copy_to_file(_Data, F, 0) ->
    ok = file:close(F);

copy_to_file(Data, F, Octets) when Octets > 0 ->
    Read = lists:min([Octets, 8000]),
    {ok, Bin} = gen_tcp:recv(Data, Read),
    ok = file:write(F, Bin),
    copy_to_file(Data, F, Octets - Read).


%% Enable a span. SS7 links in Europe pretty much always run on E1 doubleframe
%% Wait for the link to actually come up.
setup_l1(A, Span) ->
    PCM = "pcm" ++ Span,
    ok = gth:set(A, PCM,
		 [{"mode", "E1"},
		  {"status", "enabled"},
		  {"framing", "doubleframe"}]),
    receive
	{l1_message, A, {PCM, "OK"}} ->
	    ok
    after 2000 ->
	    exit("Timeout waiting for L1 to start. Is it plugged in?")
    end.

play(Host, Span, Timeslot, Filename) ->
    {ok, A} = gth:start_link(Host),
    setup_l1(A, Span),
    {ok, Bin} = file:read_file(Filename),
    {ok, Player, D} = gth:new_tcp_player(A, Span, Timeslot),
    ok = gen_tcp:send(D, Bin),
    ok = gen_tcp:close(D),
    receive
	{message_ended, A, Player} ->
	    ok
    end,
    ok = gth:bye(A).
