%%----------------------------------------------------------------------
%% Title: record definitions for the GTH API. 
%% Author: Matthias (matthias@corelatus.se)
%%
%% Purpose: All GTH-related records which are passed between API modules are
%%          here, including the ones used by gth_client_xml_parse.erl
%% 

%% Resource, e.g. CPU, board, eth_, etc. 
-record(resource, {name, 
		   details_fun,             % for getting all attributes
		   set_fun                  % for setting attributes, -> ok
		  }).

%% An api connection
-record(api_conn, {socket, timeout, peer, pid}).

%% Commands received by the GTH via the XML api
-record(cmd_tuple, {
	  command,                          % bye | delete | install | new 
                                            %  | nop | query 
				            %  | reset | set | takeover | update
	                                    %  | zero
	  id,                               % string() for jobs
	  name,                             % string() for resources
	  args                              % term()
       }).

%% Responses from the GTH. Basically a general XML tuple
-record(resp_tuple, {
	  name,                             % ok | error | job | state | event
	  attributes = [],                  % [{key, value}]
	  children = [],                    % [#resp_tuple]
	  clippings = []
	}).

%% Sources and sinks
-record(pcm_source, {module, 
		     span,                  % string(), e.g. "1a"
		     timeslot,              % integer()
		     first_bit,             % integer()
		     bandwidth}).           % integer()

-record(pcm_sink, {module,                  % quad tuple | localhost
		   span,                    % string()
		   timeslot}).              % integer()

-record(conference_source, {port}).
-record(conference_sink, {port}).

-record(rtp_source, {port}).
-record(rtp_sink,   {address,
		     port}).
-record(tcp_source, {address, port}).
-record(tcp_sink,   {address, port}).

-record(udp_sink,   {address, port}).

%% Switching
-record(simplex_cx, {source, sink}).

-record(ebs, {modules}).

%% Signalling. 

%% All signalling monitoring is a deep tuple
-record(sigmon, {
	  sources,                          % [#pcm_source]
	  ip_addr = "", ip_port = 0,        % string(), integer()
	  tag,                              % integer(), user-supplied ID
	  load_limit,                       % integer(), in percent
	  buffer_limit,                     % integer(), in bytes
	  average_period,                   % integer(), in seconds
	  header_version,                   % integer()
	  proto}).                          %   #mtp2_mon
                                            % | #f_relay_mon
                                            % | #atm_aal0_mon
                                            % | #atm_aal5_mon
                                            % | #atm_aal2_mon
                                            % | #lapd_mon
                                            % | #cas_ls_mon
                                            % | #cas_mfc_mon
                                            % | #ss5_rs_mon
                                            % | #ss5_ls_mon

-record(mtp2_mon, {fisu, dup_fisu, lssu,    % boolean()
		   dup_lssu, msu, esu,      % boolean()
		   mark_retrans,            % boolean()
		   esnf                     % boolean()
		  }).                              

-record(f_relay_mon, {
	  su,                               % boolean()
	  esu,                              % boolean()
	  timeout                           % integer(), in seconds
	 }).                              

-record(atm_aal0_mon, {
	  idle_cell,                        % boolean()
	  oam_cell,                         % boolean()
	  corrupt_cell,                     % boolean()
	  cell,                             % boolean()
	  scrambling,                       % boolean()
	  timeout                           % integer(), in seconds
	 }).


-record(atm_aal5_mon, {
	  vpi,                              % integer()
	  vci,                              % integer()
	  sdu,                              % boolean()
	  corrupt_sdu,                      % boolean()
	  scrambling,                       % boolean()
	  link_load_alarm,                  % boolean()
	  timeout                           % integer(), in seconds
	 }).

-record(atm_aal2_mon, {
	  vpi,                              % integer()
	  vci,                              % integer()
	  sdu,                              % boolean()
	  corrupt_sdu,                      % boolean()
	  scrambling,                       % boolean()
	  link_load_alarm,                  % boolean()
	  timeout                           % integer(), in seconds
	 }).

-record(lapd_mon, {
	  su,                               % boolean()
	  esu,                              % boolean()
	  timeout                           % integer(), (t203, in seconds)
	 }).

-record(cas_mfc_mon, {
	  direction                         % forward | backward
	 }).

-record(cas_ls_mon, {}).

-record(ss5_ls_mon, {}).
-record(ss5_rs_mon, {}).
-record(raw_mon, {}).

%%--------------------
%% Full-duplex signalling

-record(f_relay_layer, {
	  ip_addr, ip_port,                 % string(), integer()
	  sources,                          % [#pcm_source]
	  sinks                             % [#pcm_sink]
	 }).

-record(atm_aal0_layer, {
	  ip_addr, ip_port,                 % string(), integer()
	  sources,                          % [pcm_source()]
	  sinks,                            % [pcm_sink()]
	  scramble                          % boolean()
	 }).

-record(lapd_layer, {source :: #pcm_source{}, 
		     sink   :: #pcm_sink{}, 
		     side   :: network | user,
		     sapi   :: integer(),
		     tei    :: integer(),
		     tag    :: integer(),
		     ip_addr:: string(),
		     ip_port:: integer()}).

%% Audio recording and playback.
-record(audio_clip, {id}).
-record(player, {sources,                   % #tcp_source | [string()]
		 sink,                      % #pcm_sink
		 loop,                      % boolean()
		 nsync = 0                  % undocumented hack for TLE
		 }
	).
-record(recorder, {source,                  % pcm_source
		   sink,                    % tcp_sink
		   nsync = 0                % undocumented hack for TLE
		 }
	).

-record(wide_recorder, {span :: string(),               
			sink :: #udp_sink{},
			tag  :: 0..65536,
			opts :: []
		       }
       ).

%% Other
-record(conference, {id, sources}).
-record(tone_detector, {type, frequency, length, source}).
-record(level_detector, {source, threshold, type, period}).           
-record(sync_source, {span}).               % span, as an integer
-record(controller, {timeout, backups}).    % timeout = infinity | integer()
                                            % backups = ["apic13"]

-record(mtp2_stats, {
	  span, timeslot,
	  owner_pid,
	  n_fisu, n_lssu, n_msu, n_esu, n_rsu,
	  fisu_o, lssu_o, msu_o, esu_o, rsu_o,
	  fib_inv, bib_inv,
	  state_name,
	  cur_load, av_load, max_load,
	  n_no_sus, n_out_of_service, n_in_service, n_proc_outage, n_congested,
	  t_no_sus, t_out_of_service, t_in_service, t_proc_outage, t_congested}).

-record(lapd_stats, {
	  span, timeslot,
	  owner_pid,
	  state_name,
	  n_su, su_o, n_esu, esu_o,
	  n_up, n_down, t_up, t_down,
	  i_frames, u_frames, s_frames,
	  cur_load, av_load, max_load}).

-record(fr_stats, {
	  span, timeslot,
	  owner_pid,
	  state_name,
	  n_su, su_o, n_esu, esu_o,
	  n_up, n_down, t_up, t_down,
	  cur_load, av_load, max_load}).

-record(at0m_stats, {
	  span, timeslot,
	  owner_pid,
	  state_name,
	  n_cell, n_idle, n_oam,
	  n_sync, t_sync, t_hunt,
	  cur_load, av_load, max_load}).

-record(at2m_stats, {
	  at0m_stats,
	  vpi, vci,
	  n_sdu, sdu_o, 
	  n_cdu, cdu_o,
	  cur_load, av_load, max_load       % distinct from the AAL0 load
	 }).

-record(at5m_stats, {
	  at0m_stats, 
	  vpi, vci, 
	  n_sdu, sdu_o,
	  n_cdu, cdu_o,
	  cur_load, av_load, max_load
	 }).

-record(sigmon_empty_stats, {owner_pid, span, timeslot}).

%% speaker is here for backwards compatibility with earlier (GTH 1.x) releases.
-record(speaker, {source}).                 % #pcm_source | #pcm_sink
