%%%-------------------------------------------------------------------
%%% File    : gth.erl
%%% Author  : matthias <matthias@corelatus.se>
%%% Description : An Erlang wrapper for the XML API on a Corelatus GTH.
%%%
%%%               A corelatus GTH is a T1/E1 device which can do pretty
%%%               much anything with a T1/E1 line: detect tones, switch
%%%               timeslots, decode signalling, play recorded messages, etc.
%%%
%%%               This interface lets you interface to a GTH without
%%%               having to deal with the API socket and its XML
%%%               commands directly.
%%%
%%%               See also: www.corelatus.com/gth/api/
%%%
%%%               This code follows the GTH API documentation closely, e.g.
%%%               the API command
%%%
%%%                  <new><mtp2_monitor ....>
%%%                      <pcm_source span='1A' timeslot='9'/>
%%%                  </mtp2_monitor></new>
%%%
%%%               is implemented by the function new_mtp2_monitor/3. The
%%%               options all have the same names and values as in the
%%%               XML API, but they're coded as lists of tuples. So for
%%%               instance the option attributes in
%%%
%%%                  <mtp2_monitor msu='yes' tag='13'>
%%%
%%%               are passed to this code as
%%%
%%%                  [{"msu", "yes"}, {"tag", 13}]
%%%
%%% Example: starting MTP-2 monitoring
%%%
%%%     41> {ok, A} = gth:start_link("172.16.2.7").
%%%         {ok,<0.188.0>}
%%%     42> {ok, ID, Socket} = gth:new_mtp2_monitor(A, "1A", 16).
%%%         {ok,"m2mo21",#Port<0.2261>}
%%%
%%%     You can now read MTP-2 packets from 'Socket' by calling
%%%     gen_tcp:recv()
%%%
%%% Example: defining a clip and playing it
%%%
%%%     43> {ok, _} = gth:new_clip(A, "my clip", [1,2,3]).
%%%         {ok, "clip my clip"}
%%%     44> {ok, _} = gth:new_player(A, ["clip my clip"], "1A", 3)
%%%         {ok, "play13"}
%%%
%%% This code is shipped to customers.
%%%
%%% Copyright (c) 2009, Corelatus AB Stockholm
%%%
%%% Licence: BSD
%%%
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in the
%%%       documentation and/or other materials provided with the distribution.
%%%     * Neither the name of Corelatus nor the
%%%       names of its contributors may be used to endorse or promote products
%%%       derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY Corelatus ''AS IS'' AND ANY EXPRESS
%%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Corelatus BE LIABLE FOR ANY
%%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%
%%%
%%% To do: edoc-ify this module
%%%
%%%-------------------------------------------------------------------
-module(gth).
-behaviour(gen_server).
-include("gth_xml.hrl").

%% API
-export([start_link/1, start_link/2,
	 atomise_error_reason/1,
	 bye/1,
	 custom/3,
	 delete/2,
	 get_ip/1,
	 install/3,
	 new_clip/3,
	 new_atm_aal0_layer/3, new_atm_aal0_layer/4,
	 new_atm_aal0_monitor/3, new_atm_aal0_monitor/4,
	 new_atm_aal2_monitor/4, new_atm_aal2_monitor/5,
	 new_atm_aal5_monitor/4, new_atm_aal5_monitor/5,
	 new_cas_r2_mfc_detector/4, new_cas_r2_mfc_detector/5,
	 new_cas_r2_linesig_monitor/3, new_cas_r2_linesig_monitor/4,
	 new_connection/5, new_connection/6,
	 new_ebs/2,
	 new_fr_monitor/3, new_fr_monitor/4,
	 new_fr_layer/3,
	 new_lapd_layer/6, new_lapd_layer/7,
	 new_lapd_monitor/3, new_lapd_monitor/4,
	 new_level_detector/4, new_level_detector/6,
	 new_mtp2_monitor/3, new_mtp2_monitor/4,
	 new_player/4, new_player/5,
	 new_raw_monitor/3, new_raw_monitor/4,
	 new_recorder/3, new_recorder/4,
	 new_ss5_linesig_monitor/3, new_ss5_linesig_monitor/4,
	 new_ss5_registersig_monitor/3, new_ss5_registersig_monitor/4,
	 new_tcp_player/3, new_tcp_player/4,
	 new_wide_recorder/3,
	 new_tone_detector/3, new_tone_detector/4,
	 new_tone_detector/5, new_tone_detector/6,
	 nop/1,
	 query_jobs/3, query_jobs/2, query_job/2, query_job/3,
	 query_resource/2, query_resource/3,
	 raw_xml/2,
	 reset/1,
	 set/3,
	 takeover/2,
	 update/3,
	 zero_job/2,
	 zero_resource/2
	 ]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
	{socket :: port() | none,
	 debug_remote_ip :: hostname_or_address(),

	 xml_cmd_checker  :: fun((binary()) -> any()) | 'none',
	 xml_resp_checker :: fun((binary()) -> any()) | 'none',

	 %% pre-opened listen socket for players
	 player_ls :: {integer(), port()},
	 command_timeout = 5000 :: integer(),
	 event_dict :: dict:dictionary(),
	 resource_event_target :: pid(),
	 job_event_target :: pid(),
	 my_ip :: string()}).             %% my IP address, according to the GTH

%% Macro used in guards to decide if something is a valid event handler.
-define(IS_VALID_EVENT_HANDLER(Arg), (Arg == default orelse is_pid(Arg)) ).

%% This code supervises the GTH, and requests the GTH to supervise it.
-define(KICK_INTERVAL, 5000).

-type ok_or_error()::'ok' | {error, Reason::any()}.
-type id_or_error()::{'ok', string()} | {error, Reason::any()}.

-type keyval_list()::[{Key::atom() | string(), Value::any()}].
-type event_handler()::'default' | pid().
-type timeslot_or_timeslot_list()::1..31 | [1..31].
-type subrate()::{'subrate', Timeslot::1..31, First_bit::0..7,
		  Bandwidth::integer()}.

%% The reuse_socket option sends the signalling to an existing socket
%% instead of opening a new one.
-type monitoring_options()::[{string(), integer() | string()}
			     |{reuse_socket, port()}].

-type hostname_or_address()::inet:hostname() | inet:ip_address().
-type job()::{'job', Id::string(), Owner::string(), Counters::keyval_list()}
	     |{'job', Id::string(), Owner::string(), Modules::[string()]}
	     |{'job', Id::string(), Owner::string(), Tree::#resp_tuple{},
	       Counters::keyval_list()}.

%% API for users
-spec start_link( hostname_or_address() ) -> {'ok',pid()} | {'error', _}.
start_link(Host) ->
    start_link(Host, []).

-type startup_options() ::
	[{connect_timeout, integer()}    %% milliseconds
	 |{job_event_target, pid()}      %% callback for job-related events
	 |{resource_event_target, pid()} %% callback for resource-related events
	 |{api_port_number, integer()}   %% defaults to 2089, as per API doc

	 %% hooks for seeing all XML going to and from the GTH. Useful for
	 %% automated testing.
	 |{xml_cmd_checker,  fun((binary()) -> any())}
	 |{xml_resp_checker, fun((binary()) -> any())}
	].

%% Event handlers process GTH events. Whenever an event comes, a message
%% is sent to the given PID. The event is always a tuple:
%% -type event() :: {Type::atom(), API::pid(), Details::any()}.

%% Start_link connects to the API socket but then cedes control to
%% the newly started process. This is a bit complicated, but it allows
%% us to fail without a crash when the remote socket can't be opened.
-spec start_link( hostname_or_address(), startup_options()) ->
			{'ok', pid()} | {'error', _}.
start_link(Host, Options)
  when is_list(Options) ->
    Default = [{gth_ip, Host},
	       {connect_timeout, 5000},
	       {job_event_target, self()},
	       {resource_event_target, self()},
	       {api_port_number, 2089},
	       {xml_cmd_checker,  'none'},
	       {xml_resp_checker, 'none'}
	      ],

    Timeout = proplists:get_value(connect_timeout, Options ++ Default),
    Port_number = proplists:get_value(api_port_number, Options ++ Default),

    case gen_tcp:connect(Host, Port_number,
			 [{active, false}, binary, {packet, line}], Timeout) of
	{ok, S} ->
	    case gen_server:start_link(?MODULE,
				       [{socket, S}|Options]++ Default, []) of
		{ok, Pid} ->
		    ok = gen_tcp:controlling_process(S, Pid),
		    gen_server:call(Pid, socket_ready),
		    {ok, Pid};

		Error ->
		    Error
	    end;

	Error ->
	    Error
    end.

-spec bye(pid()) -> 'ok'.

%% Terminate gracefully. This is the only graceful way to shut down;
%% if you just kill the process, we don't attempt to send <bye/>.
bye(Pid)
  when is_pid(Pid) ->
    Result = gen_server:call(Pid, bye),
    flush(Pid),
    Result.

-spec custom(pid(), string(), keyval_list()) -> ok_or_error().
custom(Pid, Name, Attributes)
  when is_pid(Pid), is_list(Name), is_list(Attributes) ->
    gen_server:call(Pid, {custom, Name, Attributes}).

-spec delete(pid(), string()) -> ok_or_error().
delete(Pid, Id)
  when is_pid(Pid), is_list(Id) ->
    gen_server:call(Pid, {delete, Id}).

-spec get_ip(pid()) -> {ok, inet:ip_address()}.
get_ip(Pid)
  when is_pid(Pid) ->
    gen_server:call(Pid, get_ip).

-type install_fun() :: fun(() -> {binary(), fun() | eof}).
%% If called with a fun() as the third argument, the install process
%% will call fun() to obtain some data and a new fun, and then call the
%% new fun. That continues until eof is returned instead of a new fun.
-spec install(pid(), string(), binary() | {integer(), install_fun()}) ->
		     ok_or_error().
install(Pid, Name, Bin_or_fun)
  when is_pid(Pid), is_list(Name) ->
    %% Use a long timeout, install takes time
    gen_server:call(Pid, {install, Name, Bin_or_fun}, 60000).

-spec new_cas_r2_mfc_detector(pid(), string(), 1..31,
			      Direction::'forward' | 'backward',
			      monitoring_options()) ->
				     {ok, string(), port()} | {'error', any()}.
new_cas_r2_mfc_detector(Pid, Span, Timeslot, Direction, Options)
  when is_pid(Pid),
       is_integer(Timeslot),
       is_atom(Direction),
       is_list(Options) ->
    gen_server:call(Pid, {new_cas_r2_mfc_detector, Span, Timeslot,
			  Direction, Options}).
new_cas_r2_mfc_detector(Pid, Span, Timeslot, Direction) ->
    new_cas_r2_mfc_detector(Pid, Span, Timeslot, Direction, []).

-spec new_cas_r2_linesig_monitor(pid(), string(), 1..31,
				 monitoring_options()) ->
					{ok, string(), port()}
					    | {'error', any()}.
new_cas_r2_linesig_monitor(Pid, Span, Timeslot) ->
    new_cas_r2_linesig_monitor(Pid, Span, Timeslot, []).
new_cas_r2_linesig_monitor(Pid, Span, Timeslot, Options)
  when is_pid(Pid), is_integer(Timeslot), is_list(Options) ->
    gen_server:call(Pid, {new_cas_r2_linesig_monitor, Span, Timeslot, Options}).

-spec new_clip(pid(), string(), binary() | iolist()) -> id_or_error().
new_clip(Pid, Name, Bin)
  when is_pid(Pid), is_binary(Bin) ->
    gen_server:call(Pid, {new_clip, Name, Bin});

new_clip(Pid, Name, List)
  when is_list(List) ->
    new_clip(Pid, Name, list_to_binary(List)).

-spec new_connection(pid(), string(), 1..31, string(), 1..31) ->
			    id_or_error().
new_connection(Pid, S_span, S_ts, D_span, D_ts)
  when is_pid(Pid), is_integer(S_ts), is_integer(D_ts) ->
    gen_server:call(Pid, {new_connection, S_span, S_ts, D_span, D_ts}).

new_connection(Pid, S_span, S_ts, D_IP, D_span, D_ts)
  when is_pid(Pid), is_integer(S_ts), is_integer(D_ts) ->
    gen_server:call(Pid, {new_connection, S_span, S_ts, D_IP, D_span, D_ts}).

%% EBS is experimental and not formally supported. (Checked 2008-10-14)
-spec new_ebs(pid(), [string()]) -> id_or_error().
new_ebs(Pid, IPs = [H|_])
  when is_pid(Pid), is_list(H) ->
    gen_server:call(Pid, {new_ebs, IPs}).

-spec new_lapd_layer(pid(),
		     Span::string(),
		     Timeslot::1..31,
		     Side::user|network,
		     SAPI_TEI::{byte(), byte()},
		     Tag::integer(),
		     Options::keyval_list()) ->
			    {ok, string(), port()} | {error, any()}.
new_lapd_layer(Pid, Span, Ts, Side, SAPI_TEI, Tag) ->
    new_lapd_layer(Pid, Span, Ts, Side, SAPI_TEI, Tag, []).
new_lapd_layer(Pid, Span, Ts, Side, {SAPI, TEI}, Tag, Options)
  when is_pid(Pid), is_integer(Ts), is_atom(Side), is_integer(SAPI),
       is_integer(TEI), is_integer(Tag), is_list(Options) ->
    gen_server:call(Pid, {new_lapd_layer, Span, Ts, Side, {SAPI, TEI},
			  Tag, Options}).

%% Signalling_socket: a socket in {packet, 2} mode
%%
new_mtp2_monitor(Pid, Span, Ts) ->
    new_mtp2_monitor(Pid, Span, Ts, []).

-spec new_mtp2_monitor(pid(), string(),
		       timeslot_or_timeslot_list() | subrate(),
		       monitoring_options()) ->
			      {'ok', Id::string(), Signalling_socket::port()}
				  | {'error', any()}.
new_mtp2_monitor(Pid, Span, Ts, Options)
  when is_pid(Pid), is_list(Options) ->
    gen_server:call(Pid, {new_mtp2_monitor, Span, Ts, Options}).

new_lapd_monitor(Pid, Span, Ts) ->
    new_lapd_monitor(Pid, Span, Ts, []).
-spec new_lapd_monitor(pid(), string(),
		       1..31 | subrate(),
		       monitoring_options()) ->
			      {'ok', string(), port()} | {'error', any()}.
new_lapd_monitor(Pid, Span, Ts, Options)
  when is_pid(Pid), is_list(Options) ->
    gen_server:call(Pid, {new_lapd_monitor, Span, Ts, Options}).

%% The threshold is given in db
new_level_detector(Pid, Span, Ts, Threshold) ->
    new_level_detector(Pid, Span, Ts, Threshold, [], default).

-spec new_level_detector(pid(), string(), 1..31, -100..6, keyval_list(),
			 event_handler()) ->
				{'ok', string()} | {'error', any()}.
new_level_detector(Pid, Span, Ts, Threshold, Options, EH)
  when is_pid(Pid), is_integer(Ts), is_integer(Threshold), is_list(Options),
       ?IS_VALID_EVENT_HANDLER(EH) ->
    gen_server:call(Pid, {new_level_detector, Span, Ts, Threshold,
			  Options, EH}).

-spec new_atm_aal0_monitor(pid(), string(), [1..31], monitoring_options()) ->
				  {ok, string(), Signalling_socket::port()}
				      | {error, any()}.
new_atm_aal0_monitor(Pid, Span, Timeslots) ->
    new_atm_aal0_monitor(Pid, Span, Timeslots, []).

new_atm_aal0_monitor(Pid, Span, Timeslots, Options)
  when is_pid(Pid), is_list(Timeslots), is_list(Options) ->
    gen_server:call(Pid, {new_atm_aal0_monitor, Span, Timeslots, Options}).

-spec new_atm_aal2_monitor(pid(), string(), [1..31],
			   {VPI::integer(), VCI::integer()},
			   monitoring_options()) ->
				  {ok, string(), Signalling_socket::port()}
				      | {error, any()}.
new_atm_aal2_monitor(Host, Span, Timeslots, {VPI, VCI}) ->
    new_atm_aal2_monitor(Host, Span, Timeslots, {VPI, VCI}, []).

new_atm_aal2_monitor(Pid, Span, Timeslots, {VPI, VCI}, Options)
  when is_pid(Pid), is_list(Timeslots), is_integer(VPI), is_integer(VCI) ->
    gen_server:call(Pid,
		    {new_atm_aal2_monitor,
		     Span, Timeslots, {VPI, VCI}, Options}).

-spec new_atm_aal5_monitor(pid(), string(), [1..31],
			   {VPI::integer(), VCI::integer()},
			   monitoring_options()) ->
				  {ok, string(), Signalling_socket::port()}
				      | {error, any()}.

new_atm_aal5_monitor(Pid, Span, Timeslots, {VPI, VCI}) ->
    new_atm_aal5_monitor(Pid, Span, Timeslots, {VPI, VCI}, []).

new_atm_aal5_monitor(Pid, Span, Timeslots, {VPI, VCI}, Options)
  when is_pid(Pid),
       is_list(Timeslots),
       is_integer(VPI),
       is_integer(VCI),
       is_list(Options) ->
    gen_server:call(Pid,
		    {new_atm_aal5_monitor, Span, Timeslots, {VPI, VCI},
		     Options}).

-spec new_fr_monitor(pid(), string(), [1..31], monitoring_options()) ->
			    {ok, string(), Signalling_socket::port()}
				| {error, any()}.
new_fr_monitor(Pid, Span, Timeslots) ->
    new_fr_monitor(Pid, Span, Timeslots, []).
new_fr_monitor(Pid, Span, Timeslots, Options)
  when is_pid(Pid), is_list(Timeslots), is_list(Options) ->
    gen_server:call(Pid, {new_fr_monitor, Span, Timeslots, Options}).

%% Frame relay layers are undocumented and unsupported (checked 2008-05-10).
new_fr_layer(Pid, Span, Timeslots)
  when is_pid(Pid), is_list(Timeslots) ->
    gen_server:call(Pid, {new_fr_layer, Span, Timeslots}).

%% ATM layers are undocumented and unsupported (checked 2008-05-12).
new_atm_aal0_layer(Pid, Span, Timeslots) ->
    new_atm_aal0_layer(Pid, Span, Timeslots, []).
new_atm_aal0_layer(Pid, Span, Timeslots, Options)
  when is_pid(Pid), is_list(Timeslots), is_list(Options) ->
    gen_server:call(Pid, {new_atm_aal0_layer, Span, Timeslots, Options}).

-spec new_player(pid(), Clips::[string()], Span::string(), Ts::1..31) ->
			id_or_error().
new_player(Pid, Clips, Span, Ts) ->
    new_player(Pid, Clips, Span, Ts, false).

new_player(Pid, Clips, Span, Ts, Loop)
  when is_pid(Pid),
       is_boolean(Loop),
       is_integer(Ts),
       is_list(Clips),
       Clips =/= [] ->
    gen_server:call(Pid, {new_player, Clips, Span, Ts, Loop}).

%% Raw monitors are experimental (unsupported & undocumented, 2011-12-09)
-spec new_raw_monitor(pid(), string(), 0..31, monitoring_options()) ->
			 {ok, string(), Raw_socket::port()} | {error, any()}.
new_raw_monitor(Pid, Span, Ts) ->
    new_raw_monitor(Pid, Span, Ts, []).

new_raw_monitor(Pid, Span, Ts, Options)
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_raw_monitor, Span, Ts, Options}).


%% The 'Raw_socket' is a socket in {packet, 0} mode, delivering timeslot data.
-spec new_recorder(pid(), string(), 0..31, keyval_list()) ->
			  {ok, string(), Raw_socket::port()} | {error, any()}.
new_recorder(Pid, Span, Ts) ->
    new_recorder(Pid, Span, Ts, []).

new_recorder(Pid, Span, Ts, Options)
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_recorder, Span, Ts, Options}).

%% Options: {"tag", integer()}
%%        | {udp_address, {inet:ip_address(), Port::integer()}}
-spec new_wide_recorder(pid(), string(), keyval_list()) ->
			       {ok, string(), UDP::port()}
				   | {ok, string()}
				   | {error, any()}.
new_wide_recorder(Pid, Span, Options) when is_pid(Pid) ->
    gen_server:call(Pid, {new_wide_recorder, Span, Options}).

-spec new_ss5_linesig_monitor(pid(), string(), 1..31, monitoring_options()) ->
				     {ok, string(), Signalling_socket::port()}
					 | {error, any()}.
new_ss5_linesig_monitor(Pid, Span, Ts) ->
    new_ss5_linesig_monitor(Pid, Span, Ts, []).

new_ss5_linesig_monitor(Pid, Span, Ts, Options)
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_ss5_linesig_monitor, Span, Ts, Options}).

-spec new_ss5_registersig_monitor(pid(), string(), 1..31, monitoring_options()) ->
				     {ok, string(), Signalling_socket::port()}
					 | {error, any()}.
new_ss5_registersig_monitor(Pid, Span, Ts) ->
    new_ss5_registersig_monitor(Pid, Span, Ts, []).

new_ss5_registersig_monitor(Pid, Span, Ts, Options)
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_ss5_registersig_monitor, Span, Ts, Options}).

%% Raw_socket: a socket in {packet, 0} mode
-spec new_tcp_player(pid(), string(), 1..31, keyval_list()) ->
			    {ok, string(), Raw_socket::port()} | {error, any()}.
new_tcp_player(Pid, Span, Ts) ->
    new_tcp_player(Pid, Span, Ts, []).

new_tcp_player(Pid, Span, Ts, Options)
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_tcp_player, Span, Ts, Options}).

%% Whenever a tone is detected, {tone, self(), {Id, Name, Length}} is
%% sent to the event handler, by default the calling process.
-spec new_tone_detector(pid(), string(), 1..31, event_handler()) ->
			       id_or_error().
new_tone_detector(Pid, Span, Ts) ->
    new_tone_detector(Pid, Span, Ts, default).

new_tone_detector(Pid, Span, Ts, Event_handler)
  when is_pid(Pid), is_integer(Ts),
       ?IS_VALID_EVENT_HANDLER(Event_handler) ->
    gen_server:call(Pid, {new_tone_detector, Span, Ts, Event_handler}).

-spec new_tone_detector(pid(),
			string(), 1..31,
			Freq::integer(),   %% Hertz
			Length::integer(), %% milliseconds
			event_handler()) ->
			       id_or_error().
new_tone_detector(Pid, Span, Ts, Freq, Length) ->
    new_tone_detector(Pid, Span, Ts, Freq, Length, default).

new_tone_detector(Pid, Span, Ts, Freq, Length, Event_handler)
  when is_pid(Pid), Freq > 0, Freq < 4000,  Length > 40, Length < 10000 ->
    gen_server:call(Pid, {new_tone_detector, Span, Ts, Freq, Length,
			  Event_handler}).

-spec nop(pid()) ->  ok_or_error().
nop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, nop).

%% If called with Verbose=false, the return tuple has the ID, Owner
%% and counters/status indicators.
%%
%% If called with Verbose=true, the return tuple also has a
%% representation of the arguments used to start the job. This is
%% useful for debugging and serialisation. In this case,
%% -include("gth_api.hrl") to get the #resp_tuple{} definition.
%%
-spec query_jobs(pid(), [string()], boolean()) ->
			{ok, [{error, any()} | job()]}.
query_jobs(Pid, Ids, Verbose) when is_pid(Pid), is_boolean(Verbose) ->
    {ok, gen_server:call(Pid, {query_jobs, Ids, Verbose})}.

query_jobs(Pid, Ids) when is_pid(Pid) ->
    query_jobs(Pid, Ids, false).

query_job(Pid, Id, Verbose) ->
    case query_jobs(Pid, [Id], Verbose) of
	{ok, [Error = {error, _}]} -> Error;
	{ok, [Reply]} -> {ok, Reply}
    end.

query_job(Pid, Id) ->
    query_job(Pid, Id, false).


-spec query_resource(pid(), Name::string()) ->
			    {ok, keyval_list()}                 % normal
				| {ok, [Name::string()]}        % "inventory"
				| {ok, keyval_list(), binary()} % log queries
				| {error, any()}.
%% This uses a timeout of 15s. When fetching logs over a slow link, that
%% won't be enough. Why are you using a slow link?
query_resource(Pid, Name) ->
    gen_server:call(Pid, {query_resource, Name}, 15000).

%% A resource query which returns one value, the once specified by third arg
query_resource(Pid, Name, Attribute) when is_pid(Pid), Name =/= "inventory" ->
    case gen_server:call(Pid, {query_resource, Name}, 15000) of
	{ok, KVs} ->
	    query_resource_find(Attribute, KVs);
	{ok, KVs, _bin} ->
	    query_resource_find(Attribute, KVs);
	Error = {error, _} ->
	    Error
    end.

query_resource_find(Key, KVs) ->
    case lists:keyfind(Key, 1, KVs) of
	{_, V} -> {ok, V};
	_ -> {error, badarg}
    end.

%% Undocumented. Used for testing incorrect API commands.
-spec raw_xml(pid(), binary() | iolist()) -> any().
raw_xml(Pid, XML) when is_pid(Pid) ->
    gen_server:call(Pid, {raw_xml, XML}, 30000).

-spec set(pid(), Name::string(), Attributes::keyval_list()) -> ok_or_error().
set(Pid, Name, Attributes)
  when is_pid(Pid), is_list(Attributes) ->
    gen_server:call(Pid, {set, Name, Attributes}).

-spec reset(pid()) -> ok_or_error().
reset(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, reset).

-spec takeover(pid(), IDs::[string()]) -> ok_or_error().
takeover(_Pid, []) ->
    ok;
takeover(Pid, IDs = [H|_])
  when is_pid(Pid), is_list(H) ->
    gen_server:call(Pid, {takeover, IDs}).

-spec update(pid(), ID::string(), Attributes::keyval_list() | [string()]) ->
		    ok_or_error().
update(Pid, ID, Attributes)
  when is_pid(Pid), is_list(ID), is_list(Attributes) ->
    gen_server:call(Pid, {update, ID, Attributes}).

-spec zero_job(pid(), ID::string()) -> ok_or_error().
zero_job(Pid, Id) when is_pid(Pid) ->
    gen_server:call(Pid, {zero_job, Id}).

-spec zero_resource(pid(), ID::string()) -> ok_or_error().
zero_resource(Pid, Name) when is_pid(Pid) ->
    gen_server:call(Pid, {zero_resource, Name}).

%%====================================================================
%% gen_server callbacks

init(Options) ->
    erlang:process_flag(trap_exit, true),

    Hostname = proplists:get_value(gth_ip, Options),
    JET = proplists:get_value(job_event_target, Options),
    RET = proplists:get_value(resource_event_target, Options),
    S = proplists:get_value(socket, Options),
    ?IS_VALID_EVENT_HANDLER(JET) orelse exit(badarg),
    ?IS_VALID_EVENT_HANDLER(RET) orelse exit(badarg),

    {ok, {Addr, _Port}} = inet:sockname(S),
    IP = tl(lists:flatten(["." ++ integer_to_list(X)
			   || X <- tuple_to_list(Addr)])),

    erlang:send_after(?KICK_INTERVAL, self(), kick),

    State = #state{
      my_ip = IP,
      debug_remote_ip = Hostname,
      player_ls = listen(),
      socket = S,
      event_dict = dict:new(),

      job_event_target = JET,
      resource_event_target = RET,

      xml_cmd_checker  = proplists:get_value(xml_cmd_checker,  Options),
      xml_resp_checker = proplists:get_value(xml_resp_checker, Options)
     },

    {ok, State}.

handle_call(bye, _From, State = #state{socket = S}) ->
    _ = gth_apilib:send(S, "<bye/>"),

    %% GTH returns <ok/> in response to bye and then closes the socket.
    %% According to T/J, Erlang R11B on MS Windows sometimes drops that OK.
    %% So we ignore this error on windows.
    case next_non_event(State) of
	#resp_tuple{name = ok} ->
	    fine;
	_ ->
	    case os:type() of
		{win32, _} ->
		    error_logger:error_report(
		      "ignoring bad <bye/> on MS Windows\n");
		_ ->
		    exit(bad_bye)
	    end
    end,

    gen_tcp:close(S),
    {stop, normal, ok, State#state{socket = none}};

handle_call({custom, Name, Attributes}, _From, State) ->
    send_xml(State, xml:custom( Name, Attributes)),
    Reply = expect_ok(State),
    {reply, Reply, State};

%% Kill the job on the GTH, but also remove it from the event dictionary.
%% (only tone and level detectors live in the event dictionary, but erasing
%% something which isn't there is OK)
handle_call({delete, Id}, _From, State = #state{event_dict=Dict}) ->
    send_xml(State, xml:delete(Id)),
    Reply = expect_ok(State),
    New_dict = dict:erase(Id, Dict),
    {reply, Reply, State#state{event_dict = New_dict}};

handle_call(get_ip, _From, State = #state{socket = S}) ->
    I = case inet:peername(S) of
	    {ok, {IP, _Port}} ->
		IP;

	    X ->
		%% One legitimate way to end up here is if we lose contact
		%% with the GTH (power fail, ethernet unplugged, etc.).
		%%
		%% But Erlang R12B-4 SMP sometimes gets here even without
		%% anything being wrong. Looks like an Erlang bug. Hasn't
		%% been observed in R13B or later.
		%%
		%% My guess is that peername doesn't work on {active, once}
		%% sockets when they've just sent a message, which is why the
		%% sleep-and-try again approach usually works.
		io:fwrite("Impossible. Peername reported ~p. Retrying.\n", [X]),
		receive
		    Y -> io:fwrite("had ~p in the queue\n", [Y])
		after 0 ->
			ok
		end,
		timer:sleep(1000),
		{ok, {IP, _Port}} = inet:peername(S),
		IP
	 end,
    {reply, {ok, I}, State};

handle_call({install, Name, Bin_or_fun}, _From, State = #state{socket = S}) ->
    send_xml(State, xml:install(Name)),
    Type = case lists:member(Name, ["system_image", "failsafe_image"]) of
	       true -> "binary/filesystem";
	       _ -> "binary/file"
	   end,

    case Bin_or_fun of
	Bin when is_binary(Bin_or_fun) ->
	    ok = gth_apilib:send(S, Type, Bin);
	{Length, Fun} ->
	    ok = gen_tcp:send(S, gth_apilib:header(Type, Length)),
	    stream_install(S, Fun)
    end,

    Reply = expect_ok(State#state{command_timeout = 60000}),
    {reply, Reply, State};

handle_call({new_atm_aal0_monitor, Span, Timeslots, Options},
	    {Pid, _tag}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Timeslots,
				   "atm_aal0_monitor", Options),
    {reply, Reply, State};

handle_call({new_atm_aal2_monitor, Span, Timeslots, {VPI, VCI}, User_options},
	    {Pid, _tag}, State) ->
    Options = [{"vpi", VPI}, {"vci", VCI}|User_options],
    Reply = new_signalling_monitor(Pid, State, Span, Timeslots,
				   "atm_aal2_monitor", Options),
    {reply, Reply, State};

handle_call({new_atm_aal5_monitor, Span, Timeslots, {VPI, VCI}, User_options},
	    {Pid, _tag}, State) ->
    Options = [{"vpi", VPI}, {"vci", VCI}|User_options],
    Reply = new_signalling_monitor(Pid, State, Span, Timeslots,
				   "atm_aal5_monitor", Options),
    {reply, Reply, State};

handle_call({new_atm_aal0_layer, Span, Timeslots, User_options},
	    {Pid, _tag},
	    State = #state{my_ip = Hostname}) ->
    Options = User_options ++ [{"scrambling", "true"}],
    Scrambling = proplists:get_value("scrambling", Options),
    {Portno, L} = listen([{packet, 2}]),
    Sources = [xml:pcm_source(Span, Ts) || Ts <- Timeslots ],
    Sinks = [xml:pcm_sink(Span, Ts) || Ts <- Timeslots ],
    send_xml(State, xml:new("atm_aal0_layer",
			    [{"ip_addr", Hostname},
			     {"ip_port", Portno},
			     {"scrambling", Scrambling}],
			    [Sources, Sinks])),
    Reply = case receive_job_id(State) of
		{ok, Job} ->
		    {ok, D} = gen_tcp:accept(L, 2000),
		    ok = gen_tcp:controlling_process(D, Pid),
		    {ok, Job, D};
		X ->
		    X
	    end,
    ok = gen_tcp:close(L),
    {reply, Reply, State};

handle_call({new_cas_r2_mfc_detector, Span, Ts, Direction, User_options},
	    {Pid, _tag}, State) ->
    Options = case Direction of
		  forward -> [{"direction", "forward"}];
		  backward -> [{"direction", "backward"}]
	      end,
    Reply = new_signalling_monitor(Pid, State, Span, Ts, "cas_r2_mfc_detector",
				   Options ++ User_options),
    {reply, Reply, State};

handle_call({new_cas_r2_linesig_monitor, Span, Ts, Options}, {Pid, _}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Ts,
				   "cas_r2_linesig_monitor", Options),
    {reply, Reply, State};

handle_call({new_clip, Name, Bin}, _From, State = #state{socket = S}) ->
    XML = xml:new_clip(Name),
    ok = gth_apilib:sendv(S, [{"text/xml", XML}, {"binary/audio", Bin}]),
    possibly_check_xml(State#state.xml_cmd_checker, XML),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_connection, S_span, S_ts, D_IP, D_span, D_ts}, _From, State) ->
    send_xml(State, xml:new("connection", [],
				    [xml:pcm_source(S_span, S_ts),
				     xml:pcm_sink(D_IP, D_span, D_ts)])),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_connection, S_span, S_ts, D_span, D_ts}, _From, State) ->
    send_xml(State, xml:new("connection", [],
			    [xml:pcm_source(S_span, S_ts),
			     xml:pcm_sink(D_span, D_ts)])),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_ebs, IPs}, _From, State) ->
    M = [xml:tag("module", [{"ip_addr", IP}]) || IP <- IPs],
    send_xml(State, xml:new("ebs", [], M)),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_fr_monitor, Span, Timeslots, Options},
	    {Pid, _tag}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Timeslots,
				   "fr_monitor", Options),
    {reply, Reply, State};

handle_call({new_fr_layer, Span, Timeslots},
	    {Pid, _tag}, State = #state{my_ip = Hostname}) ->
    {Portno, L} = listen([{packet, 2}]),
    Sources = [xml:pcm_source(Span, Ts) || Ts <- Timeslots ],
    Sinks = [xml:pcm_sink(Span, Ts) || Ts <- Timeslots ],
    send_xml(State, xml:new("fr_layer",
			    [{"ip_addr", Hostname},
			     {"ip_port", Portno}],
			    [Sources, Sinks])),

    Reply = case receive_job_id(State) of
		{ok, Job} ->
		    {ok, D} = gen_tcp:accept(L, 2000),
		    ok = gen_tcp:controlling_process(D, Pid),
		    {ok, Job, D};

		{error, Reason} ->
		    {error, Reason}
	    end,
    ok = gen_tcp:close(L),
    {reply, Reply, State};


handle_call({new_lapd_layer, Span, Ts, Side, {SAPI, TEI}, Tag, Options},
	    {Pid, _tag}, State = #state{my_ip = Hostname}) ->

    Source_sink = [xml:pcm_source(Span, Ts), xml:pcm_sink(Span, Ts)],

    case proplists:get_value(reuse_socket, Options) of
	undefined ->
	    {Portno, L} = listen([{packet, 2}]),

	    send_xml(State, xml:new(
			      "lapd_layer",
			      [{"ip_addr", Hostname},
			       {"ip_port", Portno},
			       {"side", atom_to_list(Side)},
			       {"sapi", SAPI},
			       {"tag", Tag},
			       {"tei",  TEI}|Options], Source_sink)),

	    case receive_job_id(State) of
		{ok, Job} ->
		    {ok, D} = gen_tcp:accept(L, 2000),
		    ok = gen_tcp:close(L),
		    ok = gen_tcp:controlling_process(D, Pid),
		    {reply, {ok, Job, D}, State};
		{error, Reason} ->
		    ok = gen_tcp:close(L),
		    {reply, {error, Reason}, State}
	    end;

	D when is_port(D) ->
	    {ok, Portno} = inet:port(D),

	    send_xml(State, xml:new(
			      "lapd_layer",
			      [{"ip_addr", Hostname},
			       {"ip_port", Portno},
			       {"tag", Tag},
			       {"side", atom_to_list(Side)},
			       {"sapi", SAPI}, {"tei",  TEI}
			       |proplists:delete(reuse_socket,Options)],
			      Source_sink)),

	    case receive_job_id(State) of
		{ok, Job} ->
		    {reply, {ok, Job, D}, State};
		E = {error, _Reason} ->
		    {reply, E, State}
	    end
    end;

handle_call({new_lapd_monitor, Span, Ts, Opts}, {Pid, _tag}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Ts, "lapd_monitor", Opts),
    {reply, Reply, State};

handle_call({new_level_detector, Span, Ts, Threshold, Options, EH}, _From,
	    State = #state{ event_dict = ED,
			   job_event_target = JET}) ->

    Attrs = case proplists:get_value("type", Options ++ [{"type", "both"}]) of
		"backwards_compatible" ->
		    [{"level", Threshold}];
		Type ->
		    [{"threshold", Threshold}, {"type", Type}]
	    end,

    Attrs_and_period = case proplists:get_value("period", Options) of
			   undefined -> Attrs;
			   Period -> [{"period", Period}|Attrs]
		       end,

    send_xml(State, xml:new("level_detector", Attrs_and_period,
			    xml:pcm_source(Span, Ts))),

    case receive_job_id(State) of
	{ok, Job} ->
	    New_ED = case EH of
			 default ->
			     dict:store(Job, JET, ED);
			 _ ->
			     dict:store(Job, EH, ED)
		     end,
	    {reply, {ok, Job}, State#state{event_dict = New_ED}};

	E = {error, _Reason} ->
	    {reply, E, State}
    end;

handle_call({new_mtp2_monitor, Span, Ts, Opts}, {Pid, _tag}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Ts, "mtp2_monitor", Opts),
    {reply, Reply, State};

handle_call({new_player, Clips, Span, Ts, Loop}, _From, State) ->
    Attrs = [{"loop", atom_to_list(Loop)}],
    send_xml(State, xml:player(Clips, Attrs, Span, Ts)),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_raw_monitor, Span, Ts, Opts}, {Pid, _tag}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Ts, "raw_monitor", Opts),
    {reply, Reply, State};

handle_call({new_ss5_linesig_monitor, Span, Ts, Options}, {Pid, _}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Ts, "ss5_linesig_monitor",
				   Options),
    {reply, Reply, State};

handle_call({new_ss5_registersig_monitor, Span, Ts, Options},
	    {Pid, _}, State) ->
    Rep = new_signalling_monitor(Pid, State, Span, Ts,
				 "ss5_registersig_monitor", Options),
    {reply, Rep, State};

handle_call({new_tcp_player, Span, Ts, Options}, {Pid, _},
	    State = #state{my_ip = Hostname, player_ls = {Portno, L}}) ->

    send_xml(State, xml:new("player", Options,
			    [xml:tcp_source(Hostname, Portno),
			     xml:pcm_sink(Span, Ts)])),

    Reply = case receive_job_id(State) of
		{error, Reason} ->
		    {error, Reason};
		{ok, Job} ->
		    {ok, Data} = gen_tcp:accept(L, 1000),
		    ok = gen_tcp:controlling_process(Data, Pid),
		    {ok, Job, Data}
	    end,

    {reply, Reply, State};

handle_call({new_tone_detector, Span, Ts, Event_handler},
	    _From,
	    State = #state{event_dict = ED,
			   job_event_target = JET}) ->

    send_xml(State, xml:new("tone_detector", [], xml:pcm_source(Span, Ts))),
    case receive_job_id(State) of
	{ok, Id} ->
	    New_ED = case Event_handler of
			 default ->
			     dict:store(Id, JET, ED);
			 _ ->
			     dict:store(Id, Event_handler, ED)
		     end,
	    {reply, {ok, Id}, State#state{event_dict = New_ED}};

	ER = {error, _} ->
	    {reply, ER, State}
    end;

handle_call({new_tone_detector, Span, Ts, Freq, Length, Event_handler},
	    _From,
	    State = #state{event_dict = ED,
			   job_event_target = JET}) ->
    send_xml(State, xml:new("tone_detector",
			    [{"type", "custom"},
			     {"frequency", Freq},
			     {"length", Length}],
			    xml:pcm_source(Span, Ts))),
    {ok, Id} = receive_job_id(State),
    New_ED = case Event_handler of
		 default ->
		     dict:store(Id, JET, ED);
		 _ ->
		     dict:store(Id, Event_handler, ED)
		 end,
    {reply, {ok, Id}, State#state{event_dict = New_ED}};


handle_call({new_recorder, Span, Ts, Options},
	    {Pid, _tag}, State = #state{my_ip = Hostname}) ->
    {Portno, L} = listen(),
    send_xml(State, xml:recorder(Span, Ts, Hostname, Portno, Options)),

    Reply = case receive_job_id(State) of
		{error, Reason} ->
		    {error, Reason};

		{ok, Id} ->
		    case gen_tcp:accept(L, 1000) of
			{ok, Data} ->
			    ok = gen_tcp:controlling_process(Data, Pid),
			    {ok, Id, Data};
			_X ->
			    {error, accept_failed}
		    end
    end,
    gen_tcp:close(L),
    {reply, Reply, State};

handle_call({new_wide_recorder, Span, Options},
	    {Pid, _tag},
	    State = #state{my_ip = Hostname}) ->

    {UDP_host, UDP_portno, UDP_port}
	= case proplists:get_value(udp_address, Options) of
	      {{A,B,C,D}, Port} ->
		  {lists:flatten(io_lib:fwrite("~p.~p.~p.~p", [A,B,C,D])),
		   Port, none};

	      {Host, Port} when is_list(Host) ->
		  {Host, Port, none};

	      undefined ->
		  {ok, UDP} = gen_udp:open(0, [{active, false}, binary,
					       {recbuf, 120000}]),
		  ok = gen_tcp:controlling_process(UDP, Pid),
		  {ok, Portno} = inet:port(UDP),
		  {Hostname, Portno, UDP}
	  end,

    Tag = proplists:get_value("tag", Options, 0),

    send_xml(State, xml:wide_recorder(Span, UDP_host, UDP_portno, Tag)),

    Reply = case receive_job_id(State) of
		{ok, Id} when UDP_port == none ->
		    {ok, Id};

		{ok, Id} ->
		    {ok, Id, UDP_port};

		{error, Reason} ->
		    catch gen_udp:close(UDP_port),
		    {error, Reason}
	    end,
    {reply, Reply, State};

handle_call(nop, _From, State) ->
    send_xml(State, "<nop/>"),
    #resp_tuple{name='ok'} = next_non_event(State),
    {reply, ok, State};

handle_call({query_jobs, Ids, Verbose}, _From, State) ->
    send_xml(State, xml:query_jobs(Ids, Verbose)),
    #resp_tuple{name='state', children=Cs} = next_non_event(State),
    Reply = [case C of
		 #resp_tuple{name='error',
			     clippings=Clippings,
			     attributes=[{"reason", R}]} ->
		     {error, {atomise_error_reason(R), Clippings}};

		 #resp_tuple{} ->
		     gth_client_xml_parse:job_state(Verbose, C)
	     end || C <- Cs],
    {reply, Reply, State};

handle_call({query_resource, "inventory"}, _From, State) ->
    send_xml(State, xml:query_resource("inventory")),
    #resp_tuple{name='state', children=C} = next_non_event(State),
    Reply = {ok, [N || #resp_tuple{name=resource,
				   attributes=[{"name", N}]} <- C]},
    {reply, Reply, State};

handle_call({query_resource, "schedule"}, _From, State) ->
    send_xml(State, xml:query_resource("schedule")),
    #resp_tuple{name='state', children=C} = next_non_event(State),
    Reply = {ok, [{I, O} || #resp_tuple{name=job,
				   attributes=[{"id", I}, {"owner", O}]} <- C]},
    {reply, Reply, State};

%% Some resources, e.g. the application_log, return some attributes AND
%% THEN ALSO a potentially large amount of text in a separate chunk.
%% We handle that.

handle_call({query_resource, Name}, _From, State = #state{socket = S}) ->
    send_xml(State, xml:query_resource(Name)),
    #resp_tuple{name='state', children=[C]} = next_non_event(State),

    Reply = case C of
		#resp_tuple{name=resource, children=A} ->
		    KV = attributes_to_kv(A),

		    case get_text_trailer(S, Name) of
			none ->
			    {ok, KV};
			Bin ->
			    {ok, KV, Bin}
		    end;

		#resp_tuple{name='error',
			    clippings=Clippings,
			    attributes=[{"reason", "bad argument"}]} ->
		    {error, {badarg, Clippings}}
	    end,
    {reply, Reply, State};

%% Undocumented. Used for testing incorrect API commands.
handle_call({raw_xml, XML}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, XML),
    Reply = next_non_event(State#state{command_timeout=30000}),
    {reply, Reply, State};

handle_call(reset, _From, State) ->
    send_xml(State, xml:reset("cpu")),
    case expect_ok(State) of
	ok ->
	    {stop, normal, ok, State#state{socket = none}};
	Reply = {error, _} ->
	    {reply, Reply, State}
    end;

handle_call({set, Name, Attributes}, _From, State) ->
    send_xml(State, xml:set(Name, Attributes)),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call(socket_ready, _From, State = #state{socket = S}) ->
    ok = inet:setopts(S, [{active, once}]),

    send_xml(State, xml:tag("update",
			    [], xml:tag("controller",
					[{"timeout", ?KICK_INTERVAL *2}]))),

    %% In failsafe, we get an error for sending the above command.
    _ = expect_ok(State),

    {reply, ok, State};

handle_call({takeover, IDs}, _From, State) ->
    send_xml(State, xml:takeover(IDs)),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({update, "controller", Attrs}, _From, State) ->
    send_xml(State, xml:tag("update", [], xml:tag("controller", Attrs, []))),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({update, ID = "ebsw"++_, IPs}, _From, State) ->
    send_xml(State, xml:tag("update", [],
			    xml:tag("ebs", [{"id", ID}],
				    [xml:tag("module", [{"ip_addr", IP}])
				     || IP <- IPs]))),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({update, ID, KVs}, _From, State) ->
    Job_types = [
		 {"ldmo", "lapd_monitor"},
		 {"m2mo", "mtp2_monitor"},
		 {"ramo", "raw_monitor"}
		],
    case lists:keyfind(string:substr(ID, 1, 4), 1, Job_types) of
	false ->
	    {reply, bogus_job, State};

	{_, Job_type} ->
	    send_xml(State, xml:tag("update", [],
				    xml:tag(Job_type, [{"id", ID}|KVs]))),
	    Reply = expect_ok(State),
	    {reply, Reply, State}
    end;

handle_call({zero_job, Id}, _From, State) ->
    send_xml(State, xml:zero_job(Id)),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({zero_resource, Name}, _From, State) ->
    send_xml(State, xml:zero_resource(Name)),
    Reply = expect_ok(State),
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {stop, {unexpected_cast, Msg}, State}.

handle_info({tcp, Socket, Line1},
	    State = #state{socket = S})
  when Socket == S ->
    {'text/xml', Bin} = gth_apilib:active_content(Socket, Line1, 1000),
    possibly_check_xml(State#state.xml_resp_checker, Bin),
    String = binary_to_list(Bin),
    {ok, Parsed} = gth_client_xml_parse:string(String),
    #resp_tuple{name = event, children = Events} = Parsed,
    New_state = lists:foldl(fun handle_gth_event/2, State, Events),
    {noreply, New_state};

handle_info(kick, State = #state{socket = S}) ->
    F = fun() ->
		ok = gth_apilib:send(S, "<nop/>"),
		#resp_tuple{name='ok'} = next_non_event(State),
		erlang:send_after(?KICK_INTERVAL, self(), kick),
		{noreply, State}
	end,
    case (catch F()) of
	R = {noreply, State} ->
	    R;
	_ -> {stop, {"API watchdog expired", State#state.debug_remote_ip},
	      State}
    end;

handle_info({tcp_closed, S}, State = #state{socket = S}) ->
    {stop, {"API socket closed unexpectedly", State#state.debug_remote_ip},
     State#state{socket = none}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%======================================================================
%% Internal functions

%% Events from the GTH get processed here.
%%
%% They're arranged as job-related events first, then resource-related ones.

handle_gth_event(#resp_tuple{name=tone,
				 attributes=[{"detector", ID},
					     {"name", N},
					     {"length", L}]},
		 State = #state{event_dict = E}) ->
    Pid = dict:fetch(ID, E),
    Pid ! {tone, self(), {ID, N, list_to_integer(L)}},
    State;

handle_gth_event(#resp_tuple{name=level,
				 attributes=[{"detector", ID},
					     {"state", ST}]},
		 State = #state{event_dict = E}) ->
    Pid = dict:fetch(ID, E),
    Pid ! {level, self(), {ID, ST}},
    State;

handle_gth_event(#resp_tuple{name=fatality,
			     attributes=[{"id", ID}, {"reason", Reason}]},
		 State = #state{job_event_target = Pid}) ->
    Pid ! {fatality, self(), {ID, Reason}},
    State;

handle_gth_event(#resp_tuple{name=fatality, attributes=[{"id", ID}]},
		 State = #state{job_event_target = Pid}) ->
    Pid ! {fatality, self(), {ID, ""}},
    State;

handle_gth_event(#resp_tuple{name=l2_alarm,
			     attributes=[{"id", Id},
					 {"attribute", Attribute},
					 {"state", Al_st},
					 {"value", Value}]},
		 State = #state{job_event_target = Pid}) ->
    Pid ! {l2_alarm, self(), {Id, Attribute, Al_st, Value}},
    State;

handle_gth_event(#resp_tuple{name=l2_socket_alert,
			     attributes=[{"ip_addr", Addr},
					 {"ip_port", Port},
					 {"reason", Reason}]},
		 State = #state{job_event_target = Pid}) ->
    Pid ! {l2_socket_alert, self(), {Addr, Port, Reason}},
    State;

handle_gth_event(#resp_tuple{name=message_ended, attributes=[{"id", ID}]},
		 State = #state{job_event_target = Pid}) ->
    Pid ! {message_ended, self(), ID},
    State;

handle_gth_event(#resp_tuple{name = L2_message,
			     attributes=[{"id", ID}, {"value", Val}]},
		 State = #state{job_event_target = Pid})
  when L2_message == atm_message;
       L2_message == f_relay_message;
       L2_message == lapd_message;
       L2_message == mtp2_message ->
    Pid ! {L2_message, self(), {ID, Val}},
    State;

handle_gth_event(#resp_tuple{name = backup, children = Jobs},
		 State = #state{job_event_target = Pid}) ->
    IDs = [ID || #resp_tuple{name = job, attributes = [{"id", ID}]} <- Jobs],
    Pid ! {backup, self(), IDs},
    State;

%%--- resource events below

handle_gth_event(#resp_tuple{name=fault, attributes=[{"name", Name}]},
		 State = #state{job_event_target = _Pid}) ->
    error_logger:error_report(
      {"FAULT: GTH reported a fault. We will probably die now\n", Name}),
    State;

handle_gth_event(#resp_tuple{name=alarm,
			     attributes=[{"name", Name},
					{"attribute", Attr},
					{"state", St},
					{"value", Value}]},
		 State = #state{job_event_target = Pid}) ->
    Pid ! {alarm, self(), {Name, Attr, St, Value}},
    State;

handle_gth_event(#resp_tuple{name=alert, clippings=C, attributes=A},
		 State = #state{resource_event_target = Pid}) ->
    [{Key, Reason}] = A,
    %% Prior to 34a, the client parser returned an atom 'reason', not a string
    true = lists:member(Key, ["reason", reason]),
    Pid ! {alert, self(), {Reason, C}},
    State;

handle_gth_event(#resp_tuple{name=info, attributes=A},
		 State = #state{resource_event_target = Pid}) ->
    [{"reason", Reason}] = A,
    Pid ! {info, self(), Reason},
    State;

handle_gth_event(#resp_tuple{name=l1_message, attributes=A},
		 State = #state{resource_event_target = Pid}) ->
    [{"name", Name}, {"state", L1_state}] = A,
    Pid ! {l1_message, self(), {Name, L1_state}},
    State;

handle_gth_event(#resp_tuple{name=sdh_message, attributes=A},
		 State = #state{resource_event_target = Pid}) ->
    [{"name", Name}, {"state", SDH_state}] = A,
    Pid ! {sdh_message, self(), {Name, SDH_state}},
    State;

handle_gth_event(#resp_tuple{name=sfp_message, attributes=A},
		 State = #state{resource_event_target = Pid}) ->
    [{"name", Name}, {"state", SFP_state}] = A,
    Pid ! {sfp_message, self(), {Name, SFP_state}},
    State;

handle_gth_event(#resp_tuple{name=slip, attributes=[{"name", Span}]},
		 State = #state{resource_event_target = Pid}) ->
    Pid ! {slip, self(), Span},
    State;

handle_gth_event(#resp_tuple{name=sync_message,
			     attributes=[{"state", L}]},
		 State = #state{resource_event_target = Pid}) ->
    Pid ! {sync_message, self(), L},
    State;

handle_gth_event(#resp_tuple{name=ebs,
			     attributes=[{"ip_addr", IP},
					 {"reason", Reason}]},
		 State = #state{resource_event_target = Pid}) ->
    Pid ! {ebs, self(), {IP, Reason}},
    State;

handle_gth_event(E = #resp_tuple{}, State) ->
    %% This clause gets called if (a future version of) the GTH sends
    %% an event this code doesn't understand. We ignore it but log the
    %% event.
    error_logger:error_report({"received an unexpected event from the GTH", E}),
    State.

attributes_to_kv(A) ->
    [{K, V} || #resp_tuple{name=attribute,
			   attributes=[{"name", K}, {"value",V}]} <- A].

listen() ->
    listen([]).

listen(Opts) ->
    listen_active([{active, false}|Opts]).

listen_active(Opts) ->
    {ok, L} = gen_tcp:listen(0, [binary, {reuseaddr, true}|Opts]),
    {ok, P} = inet:port(L),
    {P, L}.

new_signalling_monitor(Pid, State, Span, Ts, Name, Options)
  when is_pid(Pid), is_integer(Ts); is_tuple(Ts) ->
    new_signalling_monitor(Pid, State, Span, [Ts], Name, Options);

new_signalling_monitor(Pid, State, Span, Timeslots, Name, Options) ->
    Disallowed_options = ["ip_addr", "ip_port"],
    case [ X || X <- Disallowed_options, proplists:is_defined(X, Options) ] of
	[] ->
	    new_signalling_monitor_checked_options(Pid, State, Span,
						   Timeslots, Name, Options);
	_ ->
	    {error, badarg}
    end.

new_signalling_monitor_checked_options(
  Pid,
  State = #state{my_ip = Hostname},
  Span,
  Timeslots,
  Name,
  Options) ->
    Sources = case Timeslots of
		  [{subrate, Timeslot, First_bit, Bandwidth}] ->
		      xml:tag("pcm_source",
			      [{"span", Span},
			       {"timeslot", Timeslot},
			       {"bandwidth", Bandwidth},
			       {"first_bit", First_bit}]);
		  _ ->
		      [xml:pcm_source(Span, Ts) || Ts <- Timeslots ]
	      end,

    case proplists:get_value(reuse_socket, Options) of
	undefined ->
	    {Portno, L} = listen([{packet, 2}]),
	    send_xml(State, xml:new(Name, [{"ip_addr", Hostname},
					   {"ip_port", Portno}|Options],
				    Sources)),
	    case receive_job_id(State) of
		{ok, Job} ->
		    {ok, D} = gen_tcp:accept(L, 2000),
		    ok = gen_tcp:close(L),
		    ok = gen_tcp:controlling_process(D, Pid),
		    {ok, Job, D};
		{error, Reason} ->
		    ok = gen_tcp:close(L),
		    {error, Reason}
	    end;

	D when is_port(D) ->
	    {ok, Portno} = inet:port(D),
	    send_xml(State, xml:new(Name,
				    [{"ip_addr", Hostname},
				     {"ip_port", Portno}
				     |proplists:delete(reuse_socket, Options)],
				    Sources)),
	    case receive_job_id(State) of
		{ok, Job} ->
		    {ok, Job, D};
		E = {error, _Reason} ->
		    E
	    end
    end.

%% Return: #resp_tuple{}
next_non_event(State = #state{socket = S, command_timeout = T}) ->
    receive
	{tcp, S, Line1} ->
	    case gth_apilib:active_content(S, Line1, T) of
		{error, Reason} ->
		    {error, Reason};

		{'text/xml', Content} ->
		    possibly_check_xml(State#state.xml_resp_checker, Content),
		    S_content = binary_to_list(Content),
		    {ok, Parsed} = gth_client_xml_parse:string(S_content),
		    case Parsed of
			#resp_tuple{name = event, children = Events} ->
			    New_state = lists:foldl(fun handle_gth_event/2,
						    State, Events),
			    #state{} = New_state,
			    next_non_event(New_state);
			_ ->
			    Parsed
		    end
	    end;

	{tcp_closed, S} ->
	    exit(api_socket_closed_remotely)

    after T ->
	    exit(reply_timeout)
    end.

%% Return: {ok, ID} | {error, Reason}
receive_job_id(State = #state{}) ->
    case next_non_event(State) of
	#resp_tuple{name=job, attributes = [{"id", Job}]} ->
	    {ok, Job};

	#resp_tuple{name=error, attributes=[{"reason", R}|_], clippings=T} ->
	    {error, {atomise_error_reason(R), T}}
    end.

expect_ok(State = #state{}) ->
    case next_non_event(State) of
	#resp_tuple{name = error, attributes=[{"reason", R}|_], clippings=C} ->
	    {error, {atomise_error_reason(R), C}};
	#resp_tuple{name = ok} -> ok
    end.

expect_ok_or_resource(State = #state{}) ->
    case next_non_event(State) of
	#resp_tuple{name = ok} ->
	    ok;
	#resp_tuple{name = resource, attributes = [{"name", Name}]} ->
	    {ok, Name};
	#resp_tuple{name = error, attributes=[{"reason", R}|_], clippings=C} ->
	    {error, {atomise_error_reason(R), C}}
    end.

%% Map the GTH error reasons to erlang-style reasons, which are atoms.
atomise_error_reason("bad argument") -> badarg;
atomise_error_reason("busy") -> busy;
atomise_error_reason("conflict") -> conflict;
atomise_error_reason("failure") -> failure;
atomise_error_reason("no such job") -> 'no such job';
atomise_error_reason("not yet implemented") -> 'not yet implemented';
atomise_error_reason("parse") -> parse;
atomise_error_reason("refused") -> refused;
atomise_error_reason("transport") -> transport;
atomise_error_reason(Other) -> exit({"must handle error reason", Other}).

%% Consume all messages which came from this API instance
flush(Pid) ->
    receive
	{_, Pid, _} ->
	    flush(Pid)
    after 0 ->
	    done
    end.

get_text_trailer(S, Name) ->
    case lists:member(Name, ["application_log", "application_log_recent",
			     "system_log", "system_log_recent",
			     "standby_application_log",
			     "standby_system_log"]) of
	false ->
	    none;

	true ->
	    receive
		{tcp, S, <<"Content-type: text/plain\r\n">>} ->
		    get_text_trailer_body(S)
	    after 4000 ->
		    exit(timeout)
	    end
    end.

get_text_trailer_body(S) ->
    {ok, <<"Content-length: ", BLength/binary>>} = gen_tcp:recv(S, 0, 1000),
    {ok, _blank_line} = gen_tcp:recv(S, 0, 1000),
    {ok, [Length], _} = io_lib:fread("~d", binary_to_list(BLength)),
    Bin = case Length of
	      0 ->
		  <<>>;
	      _ ->
		  ok = inet:setopts(S, [{packet, 0}]),
		  {ok, B} = gen_tcp:recv(S, Length),
		  B
	  end,
    ok = inet:setopts(S, [{packet, line}, {active, once}]),
    Bin.

stream_install(_Socket, eof) ->
    done;
stream_install(Socket, Fun) when is_function(Fun) ->
    {Bin, Fun_or_eof} = Fun(),
    ok = gen_tcp:send(Socket, Bin),
    stream_install(Socket, Fun_or_eof).

possibly_check_xml('none', _) ->
    do_nothing;
possibly_check_xml(Checker_function, Bin) when is_function(Checker_function) ->
    Checker_function(Bin).

send_xml(#state{xml_cmd_checker = F, socket = S}, Io_list) ->
    ok = gth_apilib:send(S, Io_list),
    possibly_check_xml(F, Io_list).
