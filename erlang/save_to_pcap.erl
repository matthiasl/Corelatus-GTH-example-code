-module(save_to_pcap).
%% 
%% Start up signalling (MTP-2, frame relay or AAL5) monitoring on the given
%% E1 interface/timeslots and save the signal units to a file in
%% libpcap format, suitable for viewing with wireshark or tcpdump.
%%
%% The AAL5 can be either SAAL (SS7 signalling over ATM) or LLC (IP over ATM)
%%
%% PCAP file format: http://wiki.wireshark.org/Development/LibpcapFileFormat
%%
%% Typical use:
%%
%%   save_to_pcap:mtp2("172.16.2.7", "2B", 16, "/tmp/isup.pcap").
%%
%% or
%%
%%   save_to_pcap:frame_relay("172.16.2.7", "2B", lists:seq(1,15), "/tmp/gb.pcap").
%%
%% The Layer 1 setup assumes an E1 connected through a monitor point.
%% Hack the 'gth:set' line if you're using T1 and/or are connected via
%% a MUX.
%%
%% Program loops forever, you need to kill it to end it.
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
-export([mtp2/4, frame_relay/4, aal5/5, from_file/2]).

mtp2(GTH_IP, Span, Timeslot, Filename) ->
    go(GTH_IP, Span, Filename, mtp2,
       fun(A) -> gth:new_mtp2_monitor(A, Span, Timeslot) end).

frame_relay(GTH_IP, Span, Timeslots, Filename) ->
    go(GTH_IP, Span, Filename, frame_relay,
       fun(A) -> gth:new_fr_monitor(A, Span, Timeslots) end).

aal5(GTH_IP, Span, Timeslots, {VPI, VCI}, Filename) ->
    go(GTH_IP, Span, Filename, aal5,
       fun(A) -> gth:new_atm_aal5_monitor(A, Span, Timeslots, {VPI, VCI}) end).

go(GTH_IP, Span, Filename, Protocol, Fun) ->
    {ok, A} = gth:start_link(GTH_IP),
    {ok, Out} = file:open(Filename, [raw, write]),
    ok = file:write(Out, pcap_file_header(Protocol)),
    ok = gth:set(A, "pcm" ++ Span, 
		 [{"mode", "E1"}, 
		  {"framing", "doubleframe"}, 
		  {"tx_enabled", "false"}, 
		  {"monitoring", "true"}, 
		  {"status", "enabled"}]),
    {ok, _ID, D} = Fun(A),

    dump(Protocol, D, Out),
    ok = file:close(Out).

dump(Protocol, D, Out) -> 
    {ok, Packet} = gen_tcp:recv(D, 0),
    ok = file:write(Out, reformat_packet(Protocol, Packet)),
    dump(Protocol, D, Out).

%% Take a packet in GTH format, return the same packet mangled to pcap format.
%%
%% Return: iolist()
reformat_packet(mtp2, <<_Tag:16, Protocol:3, _:13, Timestamp:48, SU/binary>>) 
  when Protocol == 0 ->
    [pcap_packet_header(Timestamp, size(SU)), SU];

%% pcap/wireshark expects the CRC (FCS) to be stripped from frame relay packets
reformat_packet(frame_relay, <<_Tag:16, Protocol:3, _:13, Timestamp:48, SU/binary>>) 
  when Protocol == 2 ->
    Size = size(SU) - 2,
    <<Payload:Size/binary, _CRC:16>> = SU,
    [pcap_packet_header(Timestamp, Size), Payload];

%% The SUNATM network format expects a header:
%%
%%    <<DIR:1, 0:3, Protocol:4, VPI:8, VCI:16>>
%%
%% DIR is the direction, 1 seems to mean Network to User.
%%
%% Protocol    What is it     What VPI/VCI does it normally use?
%% -------------------------------------------------------------
%%        1    LANE           ?
%%        2    LLC encap.     ?
%%        5    ILMI           0/16
%%        6    Q.SAAL         0/5
%%
%% See sunatmpos.h in the libpcap distribution. 
%%
reformat_packet(aal5, <<_Tag:16, Protocol:3, _:13, Timestamp:48, _GFC:4, VPI:8, VCI:16, _PTC:4,
		       _CPCS_UU:8, _CPU:8, _CPCS_length:16, _CPCS_CRC:32, 
		       Payload/binary>>) 
  when Protocol == 5 ->
    PCAP_protocol = case Payload of
			_ when VPI == 0, VCI == 5 -> 6;         % SAAL
			<<16#aa, 16#aa, 16#03, _/binary>> -> 2  % LLC
		    end,
		   
    [pcap_packet_header(Timestamp, size(Payload) + 4), 
     <<1:1, 0:3, PCAP_protocol:4>>, VPI, <<VCI:16>>, Payload].

%% Write a pcap file header. The pcap header file bpf.h defines network type constants:
-define(PCAP_NETWORK_FRAME_RELAY, 107).
-define(PCAP_NETWORK_SUNATM, 123).
-define(PCAP_NETWORK_MTP2, 140).

pcap_file_header(frame_relay) -> 
    pcap_file_header(?PCAP_NETWORK_FRAME_RELAY);

pcap_file_header(mtp2) -> 
    pcap_file_header(?PCAP_NETWORK_MTP2);

pcap_file_header(aal5) -> 
    pcap_file_header(?PCAP_NETWORK_SUNATM);

pcap_file_header(Network) when is_integer(Network) -> 
    Magic = 16#a1b2c3d4,
    Major = 2,
    Minor = 4,
    GMT_to_localtime = 0,
    Sigfigs = 0,
    Snaplen = 65535,   %% the maximum allowed
    <<Magic:32, Major:16, Minor:16, GMT_to_localtime:32, 
     Sigfigs:32, Snaplen:32, Network:32>>.

pcap_packet_header(Timestamp_ms, Payload_len) ->
    Timestamp_us = (Timestamp_ms rem 1000) * 1000,
    Timestamp_s = Timestamp_ms div 1000,
    <<Timestamp_s:32, Timestamp_us:32, Payload_len:32, Payload_len:32>>.

%% This function takes a saved stream from a GTH and converts it into
%% PCAP format. It automatically detects whether the stream contains
%% MTP-2 or frame relay.
%%
%% One way to generate such a saved stream is to save GTH output with
%% netcat, e.g. netcat -l -p 1234 > /tmp/captured.raw
%%
%% Another way is to sniff the ethernet with tcpdump or wireshark and
%% then reconstruct the TCP stream with either tcpflow or with the 'follow tcp'
%% tool in wireshark. Example:
%%
%%    tcpdump -w /tmp/captured_ethernet.pcap -s 0 port 1234
%%    tcpflow -r /tmp/captured_ethernet.pcap
%%
from_file(In_filename, Out_filename) ->
    {ok, In} = file:open(In_filename, [read, binary]),
    {ok, Out} = file:open(Out_filename, [write]),
    {Number, Protocol} = guess_file_protocol(In),
    ok = file:write(Out, pcap_file_header(Protocol)),
    file_to_file(In, Out, {Number, Protocol}),
    ok = file:close(In),
    ok = file:close(Out).

%% Look at the first SU in the file and guess what protocol it is.
%% This works as long as the file only contains one protocol.
guess_file_protocol(In) ->
    {ok, <<_Size:16, _Tag:16, Proto:3, _Flags:13>>} = file:read(In, 6),
    file:position(In, bof),
    case Proto of
	0 -> mtp2;
	2 -> frame_relay;
	5 -> aal5
    end.

file_to_file(In, Out, Guess) ->
    case file:read(In, 2) of
	{ok, <<Size:16>>} ->
	    {ok, Packet} = file:read(In, Size),
	    file:write(Out, reformat_packet(Guess, Packet)),
	    file_to_file(In, Out, Guess);

	eof ->
	    done
    end.
