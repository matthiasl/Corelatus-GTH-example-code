%%----------------------------------------------------------------------
%% Parser for XML commands as used by GTH (not the client side).
%%
%% This module manipulates a parse tree produced by gth_xml_scan.erl
%%
%%
%%
%% Copyright 2002, 2003, 2006, 2008, 2009 Corelatus AB Stockholm
%%
%% Author: Matthias Lang (matthias@corelatus.com)
%%
%%----------------------------------------------------------------------
-module(gth_xml_parse).
-export([string/1]).
-export([ip_to_tuple/1]). % used by gth_ebs_dev!

%% For debugging
-export([step/0, trace/0, tracer/0, perf/1]).
-include("gth_api.hrl").

%% Default buffer sizes (in octets) and load limits (in percent) and
%% time constants (in seconds)
-define(ATM_BUFLIM, 256000).
-define(FR_BUFLIM, 256000).
-define(LAPD_BUFLIM, 256000).
-define(MTP2_BUFLIM, 256000).
-define(CAS_BUFLIM, 256000).
-define(SS5_BUFLIM, 256000).

-define(ATM_LOADLIM, 50).
-define(FR_LOADLIM, 50).
-define(LAPD_LOADLIM, 50).
-define(MTP2_LOADLIM, 50).

-define(ATM_AAL0_AVG_PERIOD, 30).
-define(ATM_AAL2_AVG_PERIOD, 30).
-define(ATM_AAL5_AVG_PERIOD, 30).
-define(FR_AVG_PERIOD, 30).
-define(LAPD_AVG_PERIOD, 30).
-define(MTP2_AVG_PERIOD, 30).

%%----------------------------------------------------------------------
%% S: string()
%% Return: {ok, #cmd_tuple} | {error, {Reason, Sub_reason}}
string(S) ->
    try 
	{[Tree], []} = gth_xml_scan:scan_and_parse(S),
	#cmd_tuple{} = checked(Tree)
    of
	Cmd = #cmd_tuple{} ->
	    {ok, Cmd}
    catch
	_:_ ->
	    error_logger:error_report(xml_command, {bad_command, S}),
	    {error, {parse, malformed_XML}}
    end.

%%======================================================================
%% Checker.
%%
%% The checker transforms the parse tree, filling in defaults and checking
%% DTD constraints.
%%
%% It's not a validating parser, but it will work on all valid input.
%%
%% Returns #cmd_tuple | {error, Reason}
%%
checked({bye,    _,_,_}) -> 
    #cmd_tuple{command = bye};

checked({custom, [{"name", Name}], C, _}) when C =/= [] ->
    Pairs = lists:map(fun map_attribute_pair/1, C),
    #cmd_tuple{command = custom, name = Name, args = Pairs};

%% There are two forms of delete:
%% <delete id='x'/> and <delete><job id='x'/></delete>
checked({delete, [{"id", Id}], [], _}) ->
    #cmd_tuple{command = delete, id = Id};

checked({delete, [], [Job], _}) ->
    {job, [{"id", Id}], [], _} = Job,
    #cmd_tuple{command = delete, id = Id};

checked({install, [{"name", Name}], [], _}) ->
    #cmd_tuple{command = install, name = Name};

checked({new, [], [C], _}) ->
    #cmd_tuple{command = new, args = new_child(C)};

checked({nop,    _,_,_}) -> 
    #cmd_tuple{command = nop};

checked({'query', [], C, _}) -> 
    F = fun({resource, [{"name", Name}], _, _}) -> {resource, Name};
	   ({job,      [{"id", Id}],     _, _}) -> {job, Id}
	end,
    #cmd_tuple{command = 'query', args = lists:map(F, C)};

checked({reset, [], C,_}) -> 
    [{resource, [{"name", Name}], [], _}] = C,
    #cmd_tuple{command= reset, args = Name};

checked({set, [{"name", Name}], C, _}) -> 
    Pairs = [_|_] = lists:map(fun map_attribute_pair/1, C),
    #cmd_tuple{command = set, name = Name, args = Pairs};

checked({takeover, [], C,_}) -> 
    F = fun({job, [{"id", Id}], [], _}) -> Id end,
    #cmd_tuple{command = takeover, args = lists:map(F, C)};

checked({undoc_action, [{"command", Cmd}], C,_}) -> 
    Pairs = lists:map(fun map_attribute_pair/1, C),
    #cmd_tuple{command = undoc_action, name = Cmd, args = Pairs};

checked({undoc_query, [{"command", Cmd}], C,_}) -> 
    Pairs = lists:map(fun map_attribute_pair/1, C),
    #cmd_tuple{command = undoc_query, name = Cmd, args = Pairs};

checked({update, [], [C],_}) -> 
    update(C);

checked({zero, [], 
	 [{resource, [{"name", Name}], [], _}], _}) ->
    #cmd_tuple{command = zero, name = Name};

checked({zero, [], 
	 [{job, [{"id", Id}], [], _}], _}) ->
    #cmd_tuple{command = zero, id = Id};

checked(_) -> 
    {error, {parse, unknown_tle}}.


%% Can update either the controller or an MTP-2 monitor
update({controller, A, [], _}) ->
    [ATimeout, ABackups] = multiple_extract(A, ["timeout", {"backups", none}]),
    Backups = case ABackups of
		  List when is_list(List) ->
		      string:tokens(ABackups, " ");
		  _ ->
		      none
	      end,

    Timeout = case ATimeout of
		  "infinity" -> infinity;
		  Number -> list_to_integer(Number)
	      end,

    #cmd_tuple{command = update, id = self,
	       args = #controller{timeout = Timeout,
				  backups = Backups}};

update({mtp2_monitor, A, [],_}) ->
    L = multiple_extract([{"ip_port", "0"}|A], mtp2_attributes()),
    {value, {_, ID}} = lists:keysearch("id", 1, A),
    #cmd_tuple{command = update, id = ID, args = mtp2_fill_in(L, [])};

update({ebs, [{"id", Id}], C, _}) ->
    F = fun({module, [{"ip_addr", IP}], [], _}) -> ip_to_tuple(IP) end,
    Modules = lists:map(F, C),
    #cmd_tuple{command = update, id = Id, args = #ebs{modules = Modules}};

update({lapd_monitor, A, [], _}) ->
    L = multiple_extract([{"ip_port", "0"}|A], lapd_attributes()),
    {value, {_, ID}} = lists:keysearch("id", 1, A),
    #cmd_tuple{command = update, id = ID, args = lapd_fill_in(L, [])}.

%%======================================================================
%% Children to <new>

atm_aal0_attributes() ->
    ["cell", "oam_cell", "corrupt_cell", "idle_cell",
     "scrambling", "load_limit", "buffer_limit",
     "average_period", "tag", "timeout",
     "ip_addr", "ip_port", "header_version"].

new_child({atm_aal0_monitor, A, C, _}) ->
    L = multiple_extract(A, atm_aal0_attributes()),
    Sources = lists:map(fun pcm_source/1, C),
    atm_aal0_fill_in(L, Sources);

new_child({atm_aal2_monitor, A, C, _}) ->
    L = multiple_extract(A, atm_aal2_attributes()),
    Sources = lists:map(fun pcm_source/1, C),
    atm_aal2_fill_in(L, Sources);

new_child({atm_aal5_monitor, A, C, _}) ->
    L = multiple_extract(A, atm_aal5_attributes()),
    Sources = lists:map(fun pcm_source/1, C),
    atm_aal5_fill_in(L, Sources);

new_child({atm_aal0_layer, A, C, _}) ->
    [Addr, Portno, Scramble] 
	= multiple_extract(A, ["ip_addr", "ip_port", "scramble"]),
    Sources = [pcm_source(X) || X = {pcm_source, _,_,_} <- C],
    Sinks   = [pcm_sink(X)   || X = {pcm_sink, _,_,_}   <- C],
    #atm_aal0_layer{sources = Sources, 
		    sinks = Sinks,  
		    ip_addr = Addr,
		    ip_port = list_to_integer(Portno),
		    scramble = not (Scramble == "false")};

new_child({atm_cpcs_monitor, A, C, _}) ->
    error_logger:error_report(xml_command, 
			      {"warning, atm_cpcs_monitor is deprecated "
			       "and will be removed in a future release"}),
    Sources = lists:map(fun pcm_source/1, C),
    [_vpi, _vci|L] = multiple_extract(A, atm_aal5_attributes()),
    atm_aal5_fill_in(["0", "5" | L], Sources);

new_child({cas_r2_linesig_monitor, A, [Source], _}) ->
    [Addr, APortno, ATag] = multiple_extract(A,  ["ip_addr", "ip_port", "tag"]),
    #sigmon{ip_addr = Addr,
	    ip_port = list_to_integer(APortno),
	    sources = [pcm_source(Source)],
	    tag = f_i_def_num(ATag, 0),
	    average_period = 60,
	    load_limit = 0,
	    header_version = 0,
	    buffer_limit = ?CAS_BUFLIM,
	    proto=#cas_ls_mon{}};

new_child({cas_r2_mfc_detector, A, [Source], _}) ->
    [Addr, APortno, ADirection, ATag] 
	= multiple_extract(A, ["ip_addr", "ip_port", "direction", "tag"]),
    Direction = case ADirection of
		    "forward" -> forward;
		    "backward" -> backward
		end,
    #sigmon{ip_addr = Addr,
	    ip_port = list_to_integer(APortno),
	    sources = [pcm_source(Source)],
	    average_period = 60,
	    load_limit = 0,
	    header_version = 0,
	    buffer_limit = ?CAS_BUFLIM,
	    tag = f_i_def_num(ATag, 0),
	    proto=#cas_mfc_mon{direction=Direction}};

new_child({clip, [{"id", ID}], [], _}) ->
    #audio_clip{id=ID};

new_child({connection, [], [Source,Sink], _})->
    #simplex_cx{source = pcm_source(Source), sink = pcm_sink(Sink)};

new_child({ebs, [], C, _}) ->
    F = fun({module, [{"ip_addr", IP}], [], _}) -> ip_to_tuple(IP) end,
    #ebs{modules = lists:map(F, C)};

%% Duplex FR is undocumented and only TX is implemented
new_child({fr_layer, A, C, _}) ->
    [IP, SPort] = multiple_extract(A, ["ip_addr", "ip_port"]),
    _Sources =  [pcm_source(X) || X = {pcm_source, _,_,_} <- C],
    Sinks    =  [pcm_sink(X)   || X = {pcm_sink, _,_,_} <- C],
    (length(_Sources)+length(Sinks) == length(C)) orelse exit("bogus sources"),
    #f_relay_layer{ip_addr = IP,
		   ip_port = list_to_integer(SPort),
		   sinks = Sinks};

new_child({fr_monitor, A, C, _}) ->
    L = multiple_extract(A, fr_attributes()),
    Sources = lists:map(fun pcm_source/1, C),
    fr_fill_in(L, Sources);

new_child({lapd_layer, A, [Source, Sink], _}) ->
    To_extract = [{"side", "network"}, {"sapi", "0"}, {"tei", "0"},
		 {"ip_addr", error}, {"ip_port", error}, {"tag", error}],
    [Side, Sapi, Tei, IP, Port, ATag] = multiple_extract(A, To_extract),
    true = (IP =/= error) and (Port =/= error),
    #lapd_layer{side = Side, sapi = list_to_integer(Sapi),
		tei = list_to_integer(Tei),
		ip_addr = IP, ip_port = list_to_integer(Port),
		tag = list_to_integer(ATag),
		source = pcm_source(Source), sink = pcm_sink(Sink)};

new_child({lapd_monitor, A, [Source], _}) ->
    L = multiple_extract(A, lapd_attributes()),
    lapd_fill_in(L, [pcm_source(Source)]);

new_child({level_detector, A, [Source], _}) ->
    {Threshold, Type, Period} = 
	case A of
	    [{"level", Lev}] -> 
		level_detector_backwards_compat(Lev);

	    _ ->
		{integer_attribute("threshold", -10, A),
		 atom_attribute("type", ["low_to_high", "high_to_low", "both"],
				both, A),
		 integer_attribute("period", 100, A)}
	end,

    case pcm_source(Source) of
	_ when Threshold > 6; Threshold < -60;
	Period < 100; Period > 10000
	->
	    exit(parse);
	PS ->
	    #level_detector{source = PS, threshold = Threshold, 
			    type = Type, period = Period}
    end;

new_child({mtp2_monitor, A, C, _}) ->
    L = multiple_extract(A, mtp2_attributes()),
    Sources = lists:map(fun pcm_source/1, C),
    mtp2_fill_in(L, Sources);

new_child({mtp2_layer, _, _, _}) ->
    {error, nyi};

new_child({player, A, C, _}) ->
    Loop = atom_attribute("loop", ["true", "false"], false, A),
    NSync = integer_attribute("nsync", 0, A),
    {Sources, [Sink]} = lists:splitwith(
			fun({N,_,_,_}) -> N =/= pcm_sink end, C),
    T_or_C = case Sources of
		 [TCP = {tcp_source,_,_,_}] ->
		     tcp_source(TCP);
		  
		 _ ->
		     Clips = lists:map(
			       fun({clip, [{"id", ID}], [], _}) -> ID end,
			       Sources),
		     (Clips =/= []) orelse exit("parse"),
		     Clips
	     end,
    #player{loop = Loop, sources = T_or_C, sink = pcm_sink(Sink), nsync= NSync};

new_child({recorder, A, [Source, Sink], _}) ->
    NSync = integer_attribute("nsync", 0, A),
    #recorder{source = pcm_source(Source), nsync=NSync, sink = tcp_sink(Sink)};

new_child({speaker_check, [], [Source], _}) ->
    #speaker{source=pcm_source(Source)};

new_child({ss5_linesig_monitor, A, [Source], _}) ->
    [Addr, APortno, ATag] = multiple_extract(A, ["ip_addr", "ip_port", "tag"]),
    #sigmon{ip_addr = Addr,
	    ip_port = list_to_integer(APortno),
	    sources = [pcm_source(Source)],
	    tag = f_i_def_num(ATag, 0),
	    average_period = 60,
	    load_limit = 0,
	    header_version = 0,
	    buffer_limit = ?SS5_BUFLIM,
	    proto=#ss5_ls_mon{}};

new_child({ss5_registersig_monitor, A, [Source], _}) ->
    [Addr, APortno, ATag] = multiple_extract(A, ["ip_addr","ip_port", "tag"]),
    #sigmon{ip_addr = Addr,
	    ip_port = list_to_integer(APortno),
	    sources = [pcm_source(Source)],
	    average_period = 60,
	    load_limit = 0,
	    header_version = 0,
	    buffer_limit = ?CAS_BUFLIM,
	    tag = f_i_def_num(ATag, 0),
	    proto=#ss5_rs_mon{}};

new_child({tone_detector, A, [Source], _}) ->
    [GT, GF, GL] = multiple_extract(A, ["type", "frequency", "length"]),
    {Type, Freq, Len} = case GT of
	       error  -> {dtmf, none, none};
	       "DTMF" -> {dtmf, none, none};
	       "custom" -> {custom, list_to_integer(GF), list_to_integer(GL)}
	   end,
    #tone_detector{source = pcm_source(Source), type = Type, 
			frequency = Freq, length = Len};

new_child({wide_recorder, A, [Sink], _}) ->
    [Span, ATag] = multiple_extract(A, ["span", {"tag", "0"}]),
    #wide_recorder{span = Span, 
		   tag = list_to_integer(ATag), 
		   sink = udp_sink(Sink)};

new_child(_) ->
    exit("new_without_job").

%-----

atm_aal5_attributes() ->
    ["vpi", "vci", "sdu", "corrupt_sdu", "scrambling", "load_limit",
     "buffer_limit", "average_period", "link_load_alarm", "tag", "timeout",
     "ip_addr", "ip_port", "header_version"].

atm_aal2_attributes() ->
    atm_aal5_attributes(). % same


fr_attributes() ->
    ["su", "esu", "ip_addr", "ip_port", "load_limit",
     "buffer_limit", "average_period", "tag", "timeout", "header_version"].

lapd_attributes() ->
    ["su", "esu", "ip_addr", "ip_port", "load_limit",
     "buffer_limit", "average_period", "timeout", "tag", "header_version"].

%% The old level detector had a 0..100 scale. It maps to the threshold like
%% this: 
%%
%%    threshold = 1.08 * limit - 96
%%
%% (analytically derived)
%%
level_detector_backwards_compat(ALev) ->
    Lev = list_to_integer(ALev),
    Threshold = (Lev * 108 - 9600) div 100,
    Clamped_threshold = lists:max([lists:min([Threshold, 6]), -60]),
    {Clamped_threshold, low_to_high, 100}.

%%======================================================================
%% Grandchildren

%% Returns just the values
multiple_extract(Att, Wanted) ->
    F = fun({W,D}) ->
		case lists:keysearch(W, 1, Att) of
		    {value, {_, V}} -> V;
		    _ -> D
		end;
	   (W) ->
		case lists:keysearch(W, 1, Att) of
		    {value, {_, V}} -> V;
		    _ -> error
		end
	   end,
    lists:map(F, Wanted).

%% Provide default values for the fill_in functions below. Yes/no version
f_i_def_yn("yes", _) -> true;
f_i_def_yn("no", _) -> false;
f_i_def_yn(error, D) -> D.

%% Numerical version
f_i_def_num(error, D) -> D;
f_i_def_num(Number, _) -> list_to_integer(Number).

atm_aal0_fill_in([Cell, OAM_cell, Corrupt_cell, Idle_cell,
		  Scrambling, Load_limit,
		  Buf_limit, Avg_period, Tag, Timeout,
		  IP_addr, IP_port, Header_version],
		 PCM = [_|_]) ->
    N_port = list_to_integer(IP_port),
    IP_addr_safe = case IP_addr of
		       IPA when is_list(IPA) -> IP_addr;
		       _ -> ""
		   end,
    ATM = #atm_aal0_mon{cell = f_i_def_yn(Cell, true),
			oam_cell = f_i_def_yn(OAM_cell, false),
			corrupt_cell = f_i_def_yn(Corrupt_cell, false),
			idle_cell = f_i_def_yn(Idle_cell, false),
			scrambling = f_i_def_yn(Scrambling, true),
			timeout = f_i_def_num(Timeout, 0)},


    #sigmon{load_limit = f_i_def_num(Load_limit, ?ATM_LOADLIM),
	    buffer_limit = f_i_def_num(Buf_limit, ?ATM_BUFLIM),
	    average_period = f_i_def_num(Avg_period, ?ATM_AAL0_AVG_PERIOD),
	    ip_addr = IP_addr_safe,
	    ip_port = N_port,
	    tag = f_i_def_num(Tag, 0),
	    proto = ATM,
	    header_version = f_i_def_num(Header_version, 0),
	    sources = PCM}.

atm_aal5_fill_in([VPI, VCI, SDU, Corrupt_SDU, Scrambling, Load_limit,
		  Buf_limit, Avg_period, Link_load_alarm, Tag, Timeout,
		  IP_addr, IP_port, Header_version],
		 PCM = [_|_]) ->
    N_port = list_to_integer(IP_port),
    IP_addr_safe = case IP_addr of
		       IPA when is_list(IPA) -> IP_addr;
		       _ -> ""
		   end,
    ATM = #atm_aal5_mon{vpi=list_to_integer(VPI), vci=list_to_integer(VCI),
			sdu = f_i_def_yn(SDU, true),
			corrupt_sdu = f_i_def_yn(Corrupt_SDU, false),
			scrambling = f_i_def_yn(Scrambling, true),
			link_load_alarm = f_i_def_yn(Link_load_alarm, false),
			timeout = f_i_def_num(Timeout, 0)},

    #sigmon{load_limit = f_i_def_num(Load_limit, ?ATM_LOADLIM),
	    buffer_limit = f_i_def_num(Buf_limit, ?ATM_BUFLIM),
	    average_period = f_i_def_num(Avg_period, ?ATM_AAL5_AVG_PERIOD),
	    ip_addr = IP_addr_safe,
	    ip_port = N_port,
	    tag = f_i_def_num(Tag, 0),
	    proto = ATM,
	    header_version = f_i_def_num(Header_version, 0),
	    sources = PCM}.

atm_aal2_fill_in([VPI, VCI, SDU, Corrupt_SDU, Scrambling, Load_limit,
		  Buf_limit, Avg_period, Link_load_alarm, Tag, Timeout,
		  IP_addr, IP_port, Header_version],
		 PCM = [_|_]) ->
    N_port = list_to_integer(IP_port),
    IP_addr_safe = case IP_addr of
		       IPA when is_list(IPA) -> IP_addr;
		       _ -> ""
		   end,
    ATM = #atm_aal2_mon{vpi=list_to_integer(VPI), vci=list_to_integer(VCI),
			sdu = f_i_def_yn(SDU, true),
			corrupt_sdu = f_i_def_yn(Corrupt_SDU, false),
			scrambling = f_i_def_yn(Scrambling, true),
			link_load_alarm = f_i_def_yn(Link_load_alarm, false),
			timeout = f_i_def_num(Timeout, 0)},

    #sigmon{load_limit = f_i_def_num(Load_limit, ?ATM_LOADLIM),
	    buffer_limit = f_i_def_num(Buf_limit, ?ATM_BUFLIM),
	    average_period = f_i_def_num(Avg_period, ?ATM_AAL2_AVG_PERIOD),
	    ip_addr = IP_addr_safe,
	    ip_port = N_port,
	    tag = f_i_def_num(Tag, 0),
	    proto = ATM,
	    header_version = f_i_def_num(Header_version, 0),
	    sources = PCM}.

fr_fill_in([Su, Esu, IP_addr, IP_port, Load_limit,
	    Buf_limit, Avg_period, Tag, Timeout, Header_version],
	   PCM = [_|_]) ->
    N_port = list_to_integer(IP_port),
    IP_addr_safe = case IP_addr of
		       IPA when is_list(IPA) -> IP_addr;
		       _ -> ""
		   end,
    FR = #f_relay_mon{esu  = f_i_def_yn(Esu, false),
		     su  = f_i_def_yn(Su, true),
		     timeout = f_i_def_num(Timeout, 0)},

    #sigmon{load_limit = f_i_def_num(Load_limit, ?FR_LOADLIM),
	    buffer_limit = f_i_def_num(Buf_limit, ?FR_BUFLIM),
	    average_period = f_i_def_num(Avg_period, ?FR_AVG_PERIOD),
	    ip_addr = IP_addr_safe,
	    ip_port = N_port,
	    tag = f_i_def_num(Tag, 0),
	    header_version = f_i_def_num(Header_version, 0),
	    proto = FR,
	    sources = PCM}.

lapd_fill_in([Su, Esu, IP_addr, IP_port, Load_limit, Buf_limit, Avg_period,
	      T203, Tag, Header_version], PCMs) ->
    N_port = list_to_integer(IP_port),
    IP_addr_safe = case IP_addr of
		       IPA when is_list(IPA) -> IP_addr;
		       _ -> ""
		   end,

    LM = #lapd_mon{su = f_i_def_yn(Su, true),
		   esu  = f_i_def_yn(Esu, false),
		   timeout = f_i_def_num(T203, 15)},

    #sigmon{load_limit = f_i_def_num(Load_limit, ?LAPD_LOADLIM),
	    buffer_limit = f_i_def_num(Buf_limit, ?LAPD_BUFLIM),
	    average_period = f_i_def_num(Avg_period, ?LAPD_AVG_PERIOD),
	    ip_addr = IP_addr_safe,
	    ip_port = N_port,
	    tag = f_i_def_num(Tag, 0),
	    header_version = f_i_def_num(Header_version, 0),
	    proto = LM,
	    sources = PCMs}.


mtp2_attributes() -> 
    ["fisu", "dup_fisu", "lssu", "dup_lssu", "msu", "esu", "ip_addr",
    "ip_port", "load_limit", "buffer_limit", "average_period", "tag",
    "mark_likely_retrans", "header_version", "esnf"].

mtp2_fill_in([Fisu, Dup_fisu, Lssu, Dup_lssu, Msu, Esu,
	      IP_addr, IP_port, Load_limit, Buf_limit,
	      Avg_period, Tag, Mark_retrans, Header_version, ESNF],
	     PCMs) ->
    N_port = list_to_integer(IP_port),
    IP_addr_safe = case IP_addr of
		       IPA when is_list(IPA) -> IP_addr;
		       _ -> ""
		   end,
    MTP2 = #mtp2_mon{fisu = f_i_def_yn(Fisu, true),
		     esu  = f_i_def_yn(Esu, false),
		     lssu = f_i_def_yn(Lssu, true),
		     dup_fisu = f_i_def_yn(Dup_fisu, false),
		     dup_lssu = f_i_def_yn(Dup_lssu, false),
		     msu = f_i_def_yn(Msu, true),
		     mark_retrans = f_i_def_yn(Mark_retrans, false),
		     esnf = f_i_def_yn(ESNF, false)},

    #sigmon{load_limit = f_i_def_num(Load_limit, ?MTP2_LOADLIM),
	    buffer_limit = f_i_def_num(Buf_limit, ?MTP2_BUFLIM),
	    average_period = f_i_def_num(Avg_period, ?MTP2_AVG_PERIOD),
	    ip_addr = IP_addr_safe,
	    ip_port = N_port,
	    tag = f_i_def_num(Tag, 0),
	    header_version = f_i_def_num(Header_version, 0),
	    proto = MTP2,
	    sources = PCMs}.

%%--------------------

%% Returns: {ok, #pcm_source, Leftover}
pcm_source({pcm_source, A, [], _}) ->
    {value, {_, Span}} = lists:keysearch("span", 1, A),
    {value, {_, Timeslot}} = lists:keysearch("timeslot", 1, A),
    Bandwidth = integer_attribute("bandwidth", 64, A),
    First_bit = integer_attribute("first_bit", 0, A),
    #pcm_source{span = pcm_id(Span), module = localhost, 
		bandwidth= Bandwidth,
		first_bit= First_bit,
		timeslot = list_to_integer(Timeslot)}.

%% Old PCM naming (first clause) allowed for backwards-compatibility.
pcm_id("pcm" ++ N) -> pcm_id(N);
pcm_id(X) -> X.

pcm_sink({pcm_sink, A, [], _}) ->
    {value, {_, Span}} = lists:keysearch("span", 1, A),
    {value, {_, Timeslot}} = lists:keysearch("timeslot", 1, A),
    Module = case lists:keysearch("ip_addr", 1, A) of
		 {value, {_, Quad}} ->
		     ip_to_tuple(Quad);
		 _ ->
		     localhost
	     end,
    #pcm_sink{span = pcm_id(Span),
	      timeslot = list_to_integer(Timeslot),
	      module = Module}.

tcp_sink({tcp_sink, A, [], _}) ->
    {value, {_, Address}} = lists:keysearch("ip_addr", 1, A),
    {value, {_, Port}} = lists:keysearch("ip_port", 1, A),
    NPort = list_to_integer(Port),
    #tcp_sink{address = ip_to_tuple(Address), port = NPort}.

tcp_source({tcp_source, A, [], _}) ->
    {value, {_, Address}} = lists:keysearch("ip_addr", 1, A),
    {value, {_, Port}} = lists:keysearch("ip_port", 1, A),
    NPort = list_to_integer(Port),
    #tcp_source{address = ip_to_tuple(Address), port = NPort}.

udp_sink({udp_sink, A, [], _}) ->
    {value, {_, Address}} = lists:keysearch("ip_addr", 1, A),
    {value, {_, Port}} = lists:keysearch("ip_port", 1, A),
    NPort = list_to_integer(Port),
    #udp_sink{address = ip_to_tuple(Address), port = NPort}.

%% Name: string()
%% Default: integer()
%% List: [{string(), string()}]
integer_attribute(Name, Default, List) ->
    case lists:keysearch(Name, 1, List) of
	{value, {_, V}} -> list_to_integer(V);
	_ -> Default
    end.

%% Name: string()
%% Possibilities: [string()]
%% Default: atom()
%% List: [{string(), string()]
atom_attribute(Name, Possibilities, Default, List) ->
    case lists:keysearch(Name, 1, List) of
	{value, {_, V}} -> 
	    true = lists:member(V, Possibilities),
	    list_to_atom(V);
	_ -> 
	    Default
    end.

%% Ip: dotted quad IP string
ip_to_tuple(Ip) ->
    {ok, Numbers = [A, B, C, D], []} = io_lib:fread("~d.~d.~d.~d", Ip),
    Fun = fun(Byte) when Byte >= 0, Byte < 256 ->
		  ok
	  end,
    lists:foreach(Fun, Numbers),
    {A, B, C, D}.

map_attribute_pair({attribute, A, [], _}) ->
    list_to_tuple(multiple_extract(A, ["name", "value"])).

%%----------------------------------------------------------------------
%% For debugging.

trace() ->
    %% Trace everything
    erlang:trace_pattern({?MODULE, '_', '_'}, true, [local]),

    %% Except, don't trace the scanner and the tracer
    erlang:trace_pattern({?MODULE, 'tracer', '_'}, false, [local]),
    erlang:trace_pattern({?MODULE, 'scan', '_'}, false, [local]),
    erlang:trace_pattern({?MODULE, 'name', '_'}, false, [local]),
    erlang:trace_pattern({?MODULE, 'string', '_'}, false, [local]),
    erlang:trace_pattern({?MODULE, 'whitespace', '_'}, false, [local]),
    erlang:trace_pattern({?MODULE, 'instring', '_'}, false, [local]),

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
    int:i(gth_xml_parse, [{i, "../include"}]),
    int:break_in(?MODULE, parse, 1).

%% Performance testing, microseconds to parse one command
%%
%% On a GTH 2.1
%%
%% String                 This parser      Old parser (33c)
%%======================================================================
%% <nop/>                  35 us            15
%% <delete id='m2mo33'/>   74 us            50
%% <new><player>10 clips  800 us           596
%%
%% Running with 'export_all' seemed to get me a 10% slowdown.
%%
perf(String) ->
    {_, AS, AU} = now(),
    Laps = 100000,
    repeat_parse(String, Laps),
    {_, BS, BU} = now(),
    io:fwrite("~p milliseconds elapsed for ~p laps\n", 
	      [(BS - AS) * 1000 + (BU - AU) div 1000, Laps]).

repeat_parse(String, N) when N > 0 -> string(String), repeat_parse(String, N-1);
repeat_parse(_, _) -> done.
