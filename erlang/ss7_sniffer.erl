%%-------------------------------------------------------------------
%% File    : ss7_sniffer.erl
%% Author  : matthias <matthias@corelatus.se>
%%
%% Description : An SS7 call sniffer.
%%
%%               A quick demo of how to use the GTH to listen in on a
%%               live SS7 link and decode enough of ISUP to show when
%%               calls start and stop.
%%
%%               If you're not connected to a live link, you can use
%%               a loopback cable and record_and_play_back:play/2 to
%%               replay data from a file.
%%
%% Typical session:
%%
%%    1> ss7_sniffer:go("172.16.2.27")
%%
%%    IAM: CIC=0 Called number=21255512
%%    ignoring address complete
%%    ignoring answer
%%    ignoring release
%%    ignoring release complete
%%    ...
%%
%% Copyright (c) 2009, Corelatus AB Stockholm
%%
%% All rights reserved.
%%
%% Licence: BSD
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
-module(ss7_sniffer).
-export([go/1, go/4]).

-spec go(inet:ip_address()) -> no_return().
go(GTH_IP) ->
    go(GTH_IP, "1A", 16, false).

%% Start up the connection to the GTH module
%% Tell the GTH to start extracting MTP-2 from the given timeslot.
%%
%% Monitoring = boolean() % true if a -20dB monitor point is used
-spec go(GTH_IP::inet:ip_address(),
	 Span::string(),
	 Timeslot::integer(),
	 Monitoring::boolean())
	-> no_return().
go(GTH_IP, Span, Timeslot, Monitoring) ->
    {ok, A} = gth:start_link(GTH_IP),
    setup_l1(A, Span, Monitoring),
    {ok, _Job, Packet_socket}
	= gth:new_mtp2_monitor(A, Span, Timeslot, [{"fisu", "no"}]),
    decode_packets(Packet_socket),
    ok = gth:bye(A).

%% Enable a span. SS7 links in Europe pretty much always run on E1 doubleframe
%% Wait for the link to actually come up.
setup_l1(A, Span, Monitoring) ->
    PCM = "pcm" ++ Span,
    ok = gth:set(A, PCM,
		 [{"mode", "E1"},
		  {"status", "enabled"},
		  {"monitoring", atom_to_list(Monitoring)},
		  {"framing", "doubleframe"}]),
    receive
	{l1_message, A, {PCM, "OK"}} ->
	    ok
    after 2000 ->
	    exit("Timeout waiting for L1 to start. Is it plugged in?")
    end.

decode_packets(D) ->
    {ok, Bin} = gen_tcp:recv(D, 0),
    <<_Tag:16, 0:8, _:8, _Timestamp:48, Payload/binary>> = Bin,
    mtp2(Payload),
    decode_packets(D).

%%======================================================================
%% The rest of is just protocol decoding, i.e. nothing GTH-specific.

mtp2(<<_BSN_BIB, _FSN_FIB, _LI, SIO, SIF_CRC/binary>>) ->
    SIF_length = byte_size(SIF_CRC) - 2,
    <<SIF:SIF_length/binary, _CRC:16>> = SIF_CRC,
    mtp3(<<SIO>>, SIF).

%% Q.704 14.2.1 and 14.2.2 defines the service codes
mtp3(<<_Sub:4, Service_indicator:4>>, <<MTP3:32/little, Rest/binary>>) ->
    <<SLS:4, OPC:14, DPC:14>> = <<MTP3:32>>,
    case Service_indicator of
	0 -> % Management
	    ignore;
	1 -> % Test/maintenance
	    ignore;
	3 -> % SCCP
	    ignore;
	5 ->
	    isup(DPC, OPC, SLS, Rest);
	9 -> % B-ISUP; similar to ISUP, but not compatible. Ignore for now.
	    ignore;
	X ->
	    io:fwrite("ignoring SU with unexpected service indicator=~p\n", [X])
    end.

%% Q.767 tells us how to decode ISUP. Annex C is the best place to start.
%%
%% We're only interested in the IAM packets, they tell us when someone's
%% setting up a call.
isup(_DPC, _OPC, _SLS, <<CIC:16/little, Type:8, Tail/binary>>) ->
    case Type of
	16#01  -> isup_iam(CIC, Tail);
	16#02  -> isup_ignore("subsequent address");
	16#06  -> isup_ignore("address complete");
	16#09  -> isup_ignore("answer");
	16#0c  -> isup_ignore("release");
	16#0e  -> isup_ignore("resume");
	16#10  -> isup_rlc(CIC, Tail);
	16#2c  -> isup_ignore("call progress");
	_  -> io:fwrite("ISUP: CIC=~p Message_type:~p Tail=~p\n",
			[CIC, Type, Tail])
    end.

%% Table C-16
isup_iam(CIC, <<_Nature_of_connection,  %% 3.23
	       _Forward_call:16,        %% 3.20
	       _Calling_party_category, %% 3.9
	       _Transmission_medium,    %% 3.35

	       2:8,   %% pointer to the number
	       Pointer_to_optional:8,
	       Parameters/binary>>) ->

    B = isup_number(Parameters),
    Skip_bits = (Pointer_to_optional + 0) * 8,
    <<_:Skip_bits, Optional/binary>> = Parameters,
    A = isup_number(Optional),
    io:fwrite("IAM: CIC=~p Calling (A) number=~s Called (B) number=~s\n",
	      [CIC, A, B]).

isup_rlc(CIC, Tail) ->
    io:fwrite("RLC: CIC=~p Tail=~p\n", [CIC, Tail]).

%% ISUP subscriber numbers are encoded nybble wise. Section C 3.7

isup_number(<<Number_length:8,
	     Odd_even:1, _Nature_of_address_indicator:7,
	     _Inn_ind:1, _Numbering_plan:3, _:4,
	     Tail/binary>>) ->
    isup_number_string(Number_length - 2, Odd_even, Tail).

isup_number_string(0, 0, _) ->
    [];
isup_number_string(1, 1, <<_:4, High:4, _/binary>>) ->
    [High + $0];
isup_number_string(Length, OE, <<Low:4, High:4, Tail/binary>>) ->
    [High + $0, Low + $0|isup_number_string(Length - 1, OE, Tail)].

isup_ignore(Name) ->
    io:fwrite("ignoring ~s\n", [Name]),
    ok.

%% eof
