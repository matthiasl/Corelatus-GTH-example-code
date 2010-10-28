%%%-------------------------------------------------------------------
%%% File    : gth.erl
%%% Author  : matthias <matthias@corelatus.se>
%%% Description : An Erlang wrapper for the XML API on a Corelatus GTH.
%%%
%%%               A corelatus GTH is a T1/E1 device which can do pretty 
%%%               much anything with a T1/E1 line. This interface lets 
%%%               you interface to a GTH without having to deal with the 
%%%               API socket and its XML commands directly.
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
%%% $Id: gth.erl,v 1.48 2010-10-28 13:08:24 matthias Exp $
%%%-------------------------------------------------------------------
-module(gth).
-behaviour(gen_server).
-include("gth_api.hrl").

%% API
-export([start_link/1, start_link/2,
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
	 new_recorder/3, new_recorder/4,
	 new_ss5_linesig_monitor/3, new_ss5_linesig_monitor/4,
	 new_ss5_registersig_monitor/3, new_ss5_registersig_monitor/4,
	 new_tcp_player/3, new_tcp_player/4,
	 new_tone_detector/3, new_tone_detector/4,
	 new_tone_detector/5, new_tone_detector/6,
	 nop/1,
	 query_job/2,
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

-record(state, {socket, 
		debug_remote_ip,
		player_ls,              %% pre-opened listen socket for players
		command_timeout = 5000,
		event_dict,
		resource_event_target,  %% target for events from resources
		job_event_target,       %% default target for events from jobs
		my_ip}).                %% my IP address, according to the GTH

%% Macro to be used in guards to decide if something is a valid 
%% event handler, i.e. {pid(), term()} 
-define(IS_VALID_EVENT_HANDLER(Arg), (Arg == default orelse is_pid(Arg)) ).

%% This code supervises the GTH, and requests the GTH to supervise it.
-define(KICK_INTERVAL, 5000).

%% API for users
start_link(Host) ->
    start_link(Host, []).

%% Options is a list. Things which can be in that list:
%%
%%   {connect_timeout, integer()}
%%   {job_event_target, pid()}
%%   {resource_event_target, pid()}
%%
%% Event handlers process GTH events. Whenever an event comes, a message
%% is sent to the given PID. The event is always a tuple:
%%
%% {Type, API_instance, Details}
%%
%% Type = atom()
%% API_instance = pid()
%% Details = Tuple
%%
%% Start_link connects to the API socket but then cedes control to
%% the newly started process. This is a bit complicated, but it allows
%% us to fail without a crash when the remote socket can't be opened.
start_link(Host, Options) 
  when is_list(Options) ->
    Default = [{gth_ip, Host},
	       {connect_timeout, 5000},
	       {job_event_target, self()},
	       {resource_event_target, self()}],

    Timeout = proplists:get_value(connect_timeout, Options ++ Default),

    case gen_tcp:connect(Host, 'GTH_API_PORT'(), 
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

%% Return: ok (and the process terminates normally)
bye(Pid)
  when is_pid(Pid) ->
    Result = gen_server:call(Pid, bye),
    flush(Pid),
    Result.

%% Name = string()
%% Attributes = [{string(), Value}]
%% Value = string() | integer()
%%
%% Return: ok | {error, Reason}
custom(Pid, Name, Attributes)
  when is_pid(Pid), is_list(Name), is_list(Attributes) ->
    gen_server:call(Pid, {custom, Name, Attributes}).    

%% Return: ok | {error, {'no such job', Human_readable_info}}
delete(Pid, Id) 
  when is_pid(Pid) ->
    gen_server:call(Pid, {delete, Id}).

%% Return: {ok, GTH_IP}
get_ip(Pid) 
  when is_pid(Pid) ->
    gen_server:call(Pid, get_ip).

%% Return: ok | {error, Reason}
%% Reason = atom()
%% Bin = binary() | {Length, Install_fun}
%% Install_fun = fun() -> {binary(), fun() | eof} 
%%
%% If called with a fun() as the third argument, the install process
%% will call fun() to obtain some data and a new fun, and then call the
%% new fun. That continues until eof is returned instead of a new fun.
install(Pid, Name, Bin_or_fun)
  when is_pid(Pid), is_list(Name) ->
    %% Use a long timeout, install takes time
    gen_server:call(Pid, {install, Name, Bin_or_fun}, 60000).

%% Options = {string(), integer() | string()}
%%         | {reuse_socket, port()}
%%
%% Direction = forward | backward
%%
%% Return: {ok, Job_id, Signalling_socket} | {error, Reason}
%%

new_cas_r2_mfc_detector(Pid, Span, Timeslot, Direction) ->
    new_cas_r2_mfc_detector(Pid, Span, Timeslot, Direction, []). 
new_cas_r2_mfc_detector(Pid, Span, Timeslot, Direction, Options) 
  when is_pid(Pid), 
       is_integer(Timeslot),
       is_atom(Direction),
       is_list(Options) ->
    gen_server:call(Pid, {new_cas_r2_mfc_detector, Span, Timeslot, 
			  Direction, Options}).

new_cas_r2_linesig_monitor(Pid, Span, Timeslot) ->
    new_cas_r2_linesig_monitor(Pid, Span, Timeslot, []). 
new_cas_r2_linesig_monitor(Pid, Span, Timeslot, Options) 
  when is_pid(Pid), is_integer(Timeslot), is_list(Options) ->
    gen_server:call(Pid, {new_cas_r2_linesig_monitor, Span, Timeslot, Options}).
 
%% Return: {ok, Job_id} | {error, Reason}
new_clip(Pid, Name, Bin) 
  when is_pid(Pid), is_binary(Bin) ->
    gen_server:call(Pid, {new_clip, Name, Bin});

new_clip(Pid, Name, List) 
  when is_list(List) ->
    new_clip(Pid, Name, list_to_binary(List)).

%% Return: {ok, Job_id} | {error, Reason}
new_connection(Pid, S_span, S_ts, D_span, D_ts) 
  when is_pid(Pid), is_integer(S_ts), is_integer(D_ts) ->
    gen_server:call(Pid, {new_connection, S_span, S_ts, D_span, D_ts}).

%% Return: {ok, Job_id} | {error, Reason}
new_connection(Pid, S_span, S_ts, D_IP, D_span, D_ts) 
  when is_pid(Pid), is_integer(S_ts), is_integer(D_ts) ->
    gen_server:call(Pid, {new_connection, S_span, S_ts, D_IP, D_span, D_ts}).

%% Return: {ok, Job_id} | {error, Reason}
%%
%% EBS is experimental and not formally supported. (Checked 2008-10-14)
new_ebs(Pid, IPs = [H|_]) 
  when is_pid(Pid), is_list(H) ->
    gen_server:call(Pid, {new_ebs, IPs}).

%% Side: user | network
%% SAPI_TEI: {integer(), integer()}
%%
%% Return: {ok, Job_id, Signalling_socket}
new_lapd_layer(Pid, Span, Ts, Side, SAPI_TEI, Tag) ->
    new_lapd_layer(Pid, Span, Ts, Side, SAPI_TEI, Tag, []).    
new_lapd_layer(Pid, Span, Ts, Side, {SAPI, TEI}, Tag, Options) 
  when is_pid(Pid), is_integer(Ts), is_atom(Side), is_integer(SAPI), 
       is_integer(TEI), is_integer(Tag), is_list(Options) ->
    gen_server:call(Pid, {new_lapd_layer, Span, Ts, Side, {SAPI, TEI},
			  Tag, Options}).

%% Ts: integer() | [integer()] | {subrate, Timeslot, First_bit, Bandwidth}
%%
%% Return: {ok, Job_id, Signalling_socket} | {error, Reason}
%%
%% Signalling_socket: a socket in {packet, 2} mode
%%
%% Options: all of the XML options, except ip_port and ip_addr, 
%%          plus {reuse_socket, S}. 
%%
%% The reuse_socket option sends the signalling to an existing socket
%% instead of opening a new one.
new_mtp2_monitor(Pid, Span, Ts) ->
    new_mtp2_monitor(Pid, Span, Ts, []).    
new_mtp2_monitor(Pid, Span, Ts, Options) 
  when is_pid(Pid), is_list(Options) ->
    gen_server:call(Pid, {new_mtp2_monitor, Span, Ts, Options}).

new_lapd_monitor(Pid, Span, Ts) ->
    new_lapd_monitor(Pid, Span, Ts, []).    
new_lapd_monitor(Pid, Span, Ts, Options) 
  when is_pid(Pid), is_list(Options) ->
    gen_server:call(Pid, {new_lapd_monitor, Span, Ts, Options}).

%% Level: integer()  % level in dB
new_level_detector(Pid, Span, Ts, Threshold) ->
    new_level_detector(Pid, Span, Ts, Threshold, [], default).
new_level_detector(Pid, Span, Ts, Threshold, Options, EH) 
  when is_pid(Pid), is_integer(Ts), is_integer(Threshold), is_list(Options),
       ?IS_VALID_EVENT_HANDLER(EH) ->
    gen_server:call(Pid, {new_level_detector, Span, Ts, Threshold, 
			  Options, EH}).

%% Return: {ok, Job_id, Signalling_socket} | {error, Reason}
new_atm_aal0_monitor(Pid, Span, Timeslots) ->
    new_atm_aal0_monitor(Pid, Span, Timeslots, []).

new_atm_aal0_monitor(Pid, Span, Timeslots, Options) 
  when is_pid(Pid), is_list(Timeslots), is_list(Options) ->
    gen_server:call(Pid, {new_atm_aal0_monitor, Span, Timeslots, Options}).

%% Return: {ok, Job_id, Signalling_socket} | {error, Reason}
new_atm_aal2_monitor(Host, Span, Timeslots, {VPI, VCI}) ->
    new_atm_aal2_monitor(Host, Span, Timeslots, {VPI, VCI}, []).

new_atm_aal2_monitor(Pid, Span, Timeslots, {VPI, VCI}, Options) 
  when is_pid(Pid), is_list(Timeslots), is_integer(VPI), is_integer(VCI) ->
    gen_server:call(Pid, 
		    {new_atm_aal2_monitor, 
		     Span, Timeslots, {VPI, VCI}, Options}).

%% Return: {ok, Job_id, Signalling_socket} | {error, Reason}
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

%% Return: {ok, Job_id, Signalling_socket} | {error, Reason}
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

%% Return: {ok, Id} | {error, Reason}
new_player(Pid, Clips, Span, Ts) ->
    new_player(Pid, Clips, Span, Ts, false).

new_player(Pid, Clips, Span, Ts, Loop) 
  when is_pid(Pid), 
       is_boolean(Loop),
       is_integer(Ts),
       is_list(Clips),
       Clips =/= [] ->
    gen_server:call(Pid, {new_player, Clips, Span, Ts, Loop}).

%% Options: always an empty list.  
%%
%% Return: {ok, Id, Raw_socket} | {error, Reason}
new_recorder(Pid, Span, Ts) ->
    new_recorder(Pid, Span, Ts, []).

new_recorder(Pid, Span, Ts, Options) 
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_recorder, Span, Ts, Options}).

%% Return {ok, Job_id, Signalling_socket}
new_ss5_linesig_monitor(Pid, Span, Ts) ->
    new_ss5_linesig_monitor(Pid, Span, Ts, []).

new_ss5_linesig_monitor(Pid, Span, Ts, Options) 
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_ss5_linesig_monitor, Span, Ts, Options}).

%% Return {ok, Job_id, Signalling_socket}
new_ss5_registersig_monitor(Pid, Span, Ts) ->
    new_ss5_registersig_monitor(Pid, Span, Ts, []).

new_ss5_registersig_monitor(Pid, Span, Ts, Options)
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_ss5_registersig_monitor, Span, Ts, Options}).

%% Options: always an empty list.
%%
%% Return {ok, Id, Raw_socket}
%%
%% Raw_socket: a socket in {packet, 0} mode
new_tcp_player(Pid, Span, Ts) ->
    new_tcp_player(Pid, Span, Ts, []).

new_tcp_player(Pid, Span, Ts, Options) 
  when is_pid(Pid), is_integer(Ts), is_list(Options) ->
    gen_server:call(Pid, {new_tcp_player, Span, Ts, Options}).

%% Make a DTMF detector
%%
%% Report = pid()
%%
%% Whenever a tone is detected, {tone, self(), {Id, Name, Length}} is 
%% sent to Pid
%%
%% Return: {ok, Id} | {error, Reason}
new_tone_detector(Pid, Span, Ts) ->
    new_tone_detector(Pid, Span, Ts, default).
    
new_tone_detector(Pid, Span, Ts, Event_handler)
  when is_pid(Pid), is_integer(Ts),
       ?IS_VALID_EVENT_HANDLER(Event_handler) ->
    gen_server:call(Pid, {new_tone_detector, Span, Ts, Event_handler}).

%% Make a custom tone detector. 
%%
%% Freq = integer()  % Hertz
%%  Len = integer()  % milliseconds
%%
%% Return: {ok, Id} | {error, Reason}
new_tone_detector(Pid, Span, Ts, Freq, Length) ->
    new_tone_detector(Pid, Span, Ts, Freq, Length, default).

new_tone_detector(Pid, Span, Ts, Freq, Length, Event_handler) 
  when is_pid(Pid), Freq > 0, Freq < 4000,  Length > 40, Length < 10000 ->
    gen_server:call(Pid, {new_tone_detector, Span, Ts, Freq, Length, 
			  Event_handler}).
  
nop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, nop).

%% Return: {ok, Info}
%%       | {error, Reason}
%%
%% Info = {job, Id, Owner, Attributes} 
%% Attributes = [{Key, Value}...]       %% For all jobs except EBS
%%            | [ [{Key, Value}], ...]  %% For EBS (experimental)
%%       
query_job(Pid, Id) when is_pid(Pid) ->
    gen_server:call(Pid, {query_job, Id}).

%% Return: {ok, [Name]}          % when querying the inventory
%%       | {ok, [{Key, Value}]}  % when querying anything else
%%
%% This uses a timeout of 15s. When fetching logs over a slow link, that 
%% won't be enough. Why are you using a slow link?
query_resource(Pid, Name) when is_pid(Pid) ->
    gen_server:call(Pid, {query_resource, Name}, 15000).

query_resource(Pid, Name, Attribute) when is_pid(Pid) ->
    case query_resource(Pid, Name) of
	{ok, Dict} ->
	    case [V || {K, V} <- Dict, K == Attribute] of
		[Value] ->
		    {ok, Value};
		_ ->
		    {error, badarg}
	    end
    end.

%% Undocumented. Used for testing incorrect API commands.
raw_xml(Pid, XML) when is_pid(Pid) ->
    gen_server:call(Pid, {raw_xml, XML}, 17000).

%% Return: ok | {error, Reason}
%%
%% Name = string()
%% Attributes = [{Key, Value}]
%% Key = string()
%% Value = string() | integer()
%%
set(Pid, Name, Attributes) 
  when is_pid(Pid), is_list(Attributes) ->
    gen_server:call(Pid, {set, Name, Attributes}).

%% Return: ok | {error, Reason}
reset(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, reset).

%% IDs = [string()]
%%
%% Return: ok | {error, Reason}
takeover(_Pid, []) ->
    ok;
takeover(Pid, IDs = [H|_]) 
  when is_pid(Pid), is_list(H) ->
    gen_server:call(Pid, {takeover, IDs}).

%% Return: ok | {error, Reason}
update(Pid, ID, Attributes) 
  when is_pid(Pid), is_list(ID), is_list(Attributes) ->
    gen_server:call(Pid, {update, ID, Attributes}).

%% Return: ok | {error, Reason}
zero_job(Pid, Id) when is_pid(Pid) ->
    gen_server:call(Pid, {zero_job, Id}).

%% Return: ok | {error, Reason}
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
        
    State = #state{my_ip = IP,
		   debug_remote_ip = Hostname,
		   player_ls = listen(),
		   socket = S, 
		   event_dict = dict:new(),
		   job_event_target = JET,
		   resource_event_target = RET},
    
    {ok, State}.

handle_call(bye, _From, State) ->
    %% The actual <bye/> is sent in terminate()
    {stop, normal, ok, State};

handle_call({custom, Name, Attributes}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:custom(Name, Attributes)),
    Reply = expect_ok(State),
    {reply, Reply, State};

%% Kill the job on the GTH, but also remove it from the event dictionary.
%% (only tone and level detectors live in the event dictionary, but erasing
%% something which isn't there is OK)
handle_call({delete, Id}, _From, State = #state{socket=S, event_dict=Dict}) ->
    ok = gth_apilib:send(S, xml:delete(Id)),
    Reply = expect_ok(State),
    New_dict = dict:erase(Id, Dict),
    {reply, Reply, State#state{event_dict = New_dict}};

handle_call(get_ip, _From, State = #state{socket = S}) ->
    I = case inet:peername(S) of
	    {ok, {IP, _Port}} -> 
		IP;

	    X ->
		%% Ok, this is clearly an Erlang bug. It happens in R12B-4 SMP.
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
    ok = gth_apilib:send(S, xml:install(Name)),
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
	    State = #state{socket = S, my_ip = Hostname}) ->
    Options = User_options ++ [{"scramble", "true"}],
    Scramble = proplists:get_value("scramble", Options),
    {Portno, L} = listen([{packet, 2}]),
    Sources = [xml:pcm_source(Span, Ts) || Ts <- Timeslots ],
    Sinks = [xml:pcm_sink(Span, Ts) || Ts <- Timeslots ],
    ok = gth_apilib:send(S, xml:new("atm_aal0_layer", 
				    [{"ip_addr", Hostname},
				     {"ip_port", Portno},
				     {"scramble", Scramble}],
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
    ok = gth_apilib:send(S, xml:new_clip(Name)),
    ok = gth_apilib:send(S, Bin),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_connection, S_span, S_ts, D_IP, D_span, D_ts}, 
	    _From, 
	    State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:new("connection", [],
				    [xml:pcm_source(S_span, S_ts),
				     xml:pcm_sink(D_IP, D_span, D_ts)])),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_connection, S_span, S_ts, D_span, D_ts}, 
	    _From, 
	    State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:new("connection", [],
				    [xml:pcm_source(S_span, S_ts),
				     xml:pcm_sink(D_span, D_ts)])),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_ebs, IPs}, _From, State = #state{socket = S}) ->
    M = [xml:tag("module", [{"ip_addr", IP}]) || IP <- IPs],
    ok = gth_apilib:send(S, xml:new("ebs", [], M)),
    Reply = receive_job_id(State),
    {reply, Reply, State};

handle_call({new_fr_monitor, Span, Timeslots, Options}, 
	    {Pid, _tag}, State) ->
    Reply = new_signalling_monitor(Pid, State, Span, Timeslots, 
				   "fr_monitor", Options),
    {reply, Reply, State};

handle_call({new_fr_layer, Span, Timeslots}, 
	    {Pid, _tag}, 	    
	    State = #state{socket = S, my_ip = Hostname}) ->
    {Portno, L} = listen([{packet, 2}]),
    Sources = [xml:pcm_source(Span, Ts) || Ts <- Timeslots ],
    Sinks = [xml:pcm_sink(Span, Ts) || Ts <- Timeslots ],
    ok = gth_apilib:send(S, xml:new("fr_layer", 
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
	    {Pid, _tag}, State = #state{socket = S, my_ip = Hostname}) ->

    Source_sink = [xml:pcm_source(Span, Ts), xml:pcm_sink(Span, Ts)],

    case proplists:get_value(reuse_socket, Options) of
	undefined ->
	    {Portno, L} = listen([{packet, 2}]),

	    ok = gth_apilib:send(S, xml:new(
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

	    ok = gth_apilib:send(S, xml:new(
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
	    State = #state{socket = S,
			   event_dict = ED, 
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

    ok = gth_apilib:send(S, xml:new("level_detector", Attrs_and_period, 
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

handle_call({new_player, Clips, Span, Ts, Loop}, 
	    _From, 
	    State = #state{socket = S}) ->
    Attrs = [{"loop", atom_to_list(Loop)}],
    ok = gth_apilib:send(S, xml:player(Clips, Attrs, Span, Ts)),
    Reply = receive_job_id(State),
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

handle_call({new_tcp_player, Span, Ts, Options},
	    {Pid, _}, 	    
	    State = #state{socket = S, my_ip = Hostname, 
			   player_ls = {Portno, L}}) ->
    
    ok = gth_apilib:send(S, xml:new("player", Options, 
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
	    State = #state{socket = S, 
			   event_dict = ED, 
			   job_event_target = JET}) ->

    ok = gth_apilib:send(S, xml:new("tone_detector", [], 
				    xml:pcm_source(Span, Ts))),
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
	    State = #state{socket = S,
			   event_dict = ED,
			   job_event_target = JET}) ->
    ok = gth_apilib:send(S, xml:new("tone_detector", 
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
	    {Pid, _tag},
	    State = #state{socket = S, my_ip = Hostname}) ->
    {Portno, L} = listen(),
    ok = gth_apilib:send(S, xml:new("recorder", Options, 
				    [xml:pcm_source(Span, Ts),
				     xml:tcp_sink(Hostname, Portno)])),

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

handle_call(nop, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, "<nop/>"),
    #resp_tuple{name='ok'} = next_non_event(State),
    {reply, ok, State};

handle_call({query_job, Id}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:query_job(Id)),
    #resp_tuple{name='state', children=[C]} 
      = next_non_event(State),
    Reply = case C of
		#resp_tuple{name='error', 
			    clippings=Clippings,
			    attributes=[{"reason", R}]} ->
		    {error, {atomise_error_reason(R), Clippings}};

		_ -> 
		    {ok, decode_job_state(C)}
	    end,
    {reply, Reply, State};

handle_call({query_resource, "inventory"}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:query_resource("inventory")),
    #resp_tuple{name='state', children=C} = next_non_event(State),
    Reply = {ok, [N || #resp_tuple{name=resource, 
				   attributes=[{"name", N}]} <- C]},
    {reply, Reply, State};

handle_call({query_resource, "schedule"}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:query_resource("schedule")),
    #resp_tuple{name='state', children=C} = next_non_event(State),
    Reply = {ok, [{I, O} || #resp_tuple{name=job, 
				   attributes=[{"id", I}, {"owner", O}]} <- C]},
    {reply, Reply, State};

%% Some resources, e.g. the application_log, return some attributes AND
%% THEN ALSO a potentially large amount of text in a separate chunk.
%% We handle that.

handle_call({query_resource, Name}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:query_resource(Name)),
    #resp_tuple{name='state', children=[C]} = next_non_event(State),
		  
    Reply = case C of
		#resp_tuple{name=resource, children=A} ->
		    KV = [{K, V} || #resp_tuple{name=attribute, 
						attributes=[{"name", K}, 
							    {"value",V}]} <- A],
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
    Reply = next_non_event(State#state{command_timeout=16000}),
    {reply, Reply, State};

handle_call(reset, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:reset("cpu")),
    case expect_ok(State) of
	ok ->
	    {stop, normal, ok, State#state{socket = none}};
	Reply = {error, _} ->
	    {reply, Reply, State}
    end;

handle_call({set, Name, Attributes}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:set(Name, Attributes)),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call(socket_ready, _From, State = #state{socket = S}) ->
    ok = inet:setopts(S, [{active, once}]),

    ok = gth_apilib:send(S, xml:tag("update", [], 
				    xml:tag("controller", 
					    [{"timeout", ?KICK_INTERVAL *2}]))),
    
    %% In failsafe, we get an error for sending the above command.
    _ = expect_ok(State),

    {reply, ok, State};

handle_call({takeover, IDs}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:takeover(IDs)),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({update, "controller", Attrs}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:tag("update", [], 
			       xml:tag("controller", Attrs, []))),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({update, ID = "ebsw"++_, IPs}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, 
			 xml:tag("update", [], 
				 xml:tag("ebs", [{"id", ID}], 
					 [xml:tag("module", [{"ip_addr", IP}])
					  || IP <- IPs]))),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({zero_job, Id}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:zero_job(Id)),
    Reply = expect_ok(State),
    {reply, Reply, State};

handle_call({zero_resource, Name}, _From, State = #state{socket = S}) ->
    ok = gth_apilib:send(S, xml:zero_resource(Name)),
    Reply = expect_ok(State),
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {stop, {unexpected_cast, Msg}, State}.

handle_info({tcp, Socket, Line1}, 
	    State = #state{socket = S}) 
  when Socket == S ->
    {'text/xml', Bin} = gth_apilib:active_content(Socket, Line1, 1000),
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

handle_info(_Info, State) ->
    {noreply, State}.

terminate(normal, #state{socket = none}) ->
    ok;

terminate(_Reason, State = #state{socket = S}) ->
    _ = gth_apilib:send(S, "<bye/>"),

    %% GTH returns <ok/> in response to bye and then closes the socket.
    %% According to T/J, Erlang on MS Windows sometimes drops that OK.
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
  State = #state{socket = S, my_ip = Hostname}, 
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
	    ok = gth_apilib:send(S, xml:new(Name, [{"ip_addr", Hostname},
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
	    ok = gth_apilib:send(
		   S, xml:new(Name, 
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
		    %% Commented-out line is a hook we can use to validate XML
		    %% release_test:xml_validate(Content),
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
	    end

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

%% Map the GTH error reasons to erlang-style reasons, which are atoms.
atomise_error_reason("bad argument") -> badarg;
atomise_error_reason("busy") -> busy;
atomise_error_reason("conflict") -> conflict;
atomise_error_reason("failure") -> failure;
atomise_error_reason("no such job") -> 'no such job';
atomise_error_reason("parse") -> parse;
atomise_error_reason("refused") -> refused;
atomise_error_reason("transport") -> transport;
atomise_error_reason(Other) -> exit({"must handle error reason", Other}).

'GTH_API_PORT'() -> 
    2089.

%% Query of "self" only returns an ID
decode_job_state(#resp_tuple{name=job, attributes=[{"id", I}]}) ->
    {job, I, I, []};

%% Query of a controller 
decode_job_state(#resp_tuple{name=controller, attributes=A}) ->
    ID = case proplists:get_value("id", A) of
	     undefined -> "";     %% Prior to 33a, no ID in apic query return
	     X -> X
	 end,
    {job, ID, ID, A};

%% Some jobs have attributes, but they're not nested
decode_job_state(#resp_tuple{name=Monitor,
			     attributes=[{"id", I}|Possible_owner],
			     children=C})
when Monitor == atm_aal0_monitor;
     Monitor == atm_aal2_monitor;
     Monitor == atm_aal5_monitor;
     Monitor == cas_r2_linesig_monitor;
     Monitor == cas_r2_mfc_detector;
     Monitor == f_relay_monitor;
     Monitor == lapd_monitor;
     Monitor == mtp2_monitor;
     Monitor == ss5_linesig_monitor;
     Monitor == ss5_registersig_monitor
     ->
    KVs = [{K,V} || #resp_tuple{name=attribute,
				attributes=[{"name", K}, {"value", V}]}
		       <- C],
    Owner = case Possible_owner of
		[{"owner", O}] -> O;
		[] -> no_owner_prior_to_33a
	    end,
    {job, I, Owner, KVs};

decode_job_state(#resp_tuple{name=player,
			     attributes=[{"id", I}, {"owner", O}|T]}) ->
    {job, I, O, T};


%% EBS has nested information
decode_job_state(#resp_tuple{name=ebs,
			     attributes=[{"id", I}, {"owner", O}],
			     children=C}) ->
    Info = [Attrs || #resp_tuple{name = module, attributes = Attrs} <- C],
    {job, I, O, Info};

%% Catch-all for the rest, which don't have any attributes
decode_job_state(#resp_tuple{name=job,
			     attributes=[{"id", I}, {"owner", O}],
			     children=C}) ->
    case C of
	[] -> ok;
	undefined -> ok;
	_ -> 
	    error_logger:error_report(
	      {"Discarding info in decode_job_state", I, C})
    end,
    {job, I, O, []}.

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
    ok = inet:setopts(S, [{packet, 0}]),
    {ok, [Length], _} = io_lib:fread("~d", binary_to_list(BLength)),
    {ok, Bin} = gen_tcp:recv(S, Length),
    ok = inet:setopts(S, [{packet, line}, {active, once}]),
    Bin.

stream_install(_Socket, eof) ->
    done;
stream_install(Socket, Fun) when is_function(Fun) ->
    {Bin, Fun_or_eof} = Fun(),
    ok = gen_tcp:send(Socket, Bin),
    stream_install(Socket, Fun_or_eof).
