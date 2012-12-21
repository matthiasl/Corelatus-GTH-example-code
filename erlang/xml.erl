-module(xml).
%%%
%%% A little library of routines to help with XML when talking to a GTH.
%%%

%% Generate tags
-export([attribute/2,
	 clip/1,
	 connection/4,
	 custom/2,
	 delete/1,
	 disable/1,
	 enable/2,
	 install/1,
	 job/1,
	 map/2,
	 new/3,
	 new_clip/1,
	 pcm_sink/2,
	 pcm_sink/3,
	 pcm_source/2,
	 player/3, player/4,
	 query_jobs/2, query_job/1,
	 query_resource/1,
	 recorder/4, recorder/5,
	 reset/1,
	 set/2,
	 tag/3, tag/2,
	 takeover/1,
	 tcp_sink/2, tcp_source/2,
	 unmap/1,
	 wide_recorder/4,
	 zero_job/1,
	 zero_resource/1]).

%%----------------------------------------------------------------------
%% XML generation

attribute(N, V) when is_list(N), is_list(V) ->
    tag("attribute", [{"name", N}, {"value", V}], "");

attribute(N, V) when is_list(N), is_integer(V) ->
    tag("attribute", [{"name", N}, {"value", integer_to_list(V)}], "").

clip(Id) when is_list(Id) ->
    tag("clip", [{"id", Id}], []).

connection(SP, ST, DP, DT) ->
    new("connection", [], [pcm_source(SP, ST), pcm_sink(DP, DT)]).

custom(Name, Attrs) when is_list(Name), is_list(Attrs) ->
    tag("custom", [{"name", Name}],
	[ attribute(N, stringify(V)) || {N, V} <- Attrs]).

disable(Name) ->
    tag("disable", [{"name", Name}]).

enable(Name, KVs) ->
    tag("enable", [{"name", Name}], [attribute(K, V) || {K, V} <- KVs]).

delete(Id) when is_list(Id) ->
    ["<delete id=\"", Id, "\"/>"].

install(Name) when is_list(Name) ->
    ["<install name=\"", Name, "\"/>"].

job(Id) when is_list(Id) ->
    ["<job id=\"", Id, "\"/>"].

map(_Target_type = pcm_source, Name) ->
    tag("map", [{"target_type", "pcm_source"}],
	tag("sdh_source", [{"name", Name}])).

new(Child, Attrs, Children) ->
    tag("new", [], tag(Child, Attrs, Children)).

new_clip(Id) when is_list(Id) ->
    tag("new", [], tag("clip", [{"id", Id}], [])).

player(Clip_list, Span, Timeslot) ->
    player(Clip_list, [], Span, Timeslot).

player(Clipnames, Attrs, Span, Timeslot)
  when is_list(Clipnames), is_list(hd(Clipnames)) ->
    Clips = [tag("clip", [{"id", Clipname}], "") || Clipname <- Clipnames],
    Sink = pcm_sink(Span, Timeslot),
    tag("new", [], tag("player", Attrs, [Clips, Sink])).

pcm_source(Span, Timeslot) when is_integer(Timeslot) ->
    ST = integer_to_list(Timeslot),
    tag("pcm_source", [{"span", Span}, {"timeslot", ST}], []).

pcm_sink(Span, Timeslot) when is_integer(Timeslot) ->
    ST = integer_to_list(Timeslot),
    tag("pcm_sink", [{"span", Span}, {"timeslot", ST}], []).

%% Experimental version for EBS. Experimental as at 2008-09-23
pcm_sink(IP, Span, Timeslot) when is_integer(Timeslot) ->
    ST = integer_to_list(Timeslot),
    tag("pcm_sink", [{"ip_addr", IP}, {"span", Span}, {"timeslot", ST}], []).

query_jobs(Ids, Verbose) ->
    Attrs = case Verbose of
		true -> [{"verbose", "true"}];
		false -> []
	    end,
    tag("query", Attrs, [tag("job", [{"id", Id}]) || Id <- Ids]).

query_job(Id) ->
    query_jobs([Id], false).

query_resource(Name) ->
    tag("query", [], tag("resource", [{"name", Name}])).

recorder(Span, Timeslot, Host, Port) ->
    recorder(Span, Timeslot, Host, Port, []).

recorder(Span, Timeslot, Host, Port, All_opts) ->
    Sink = case lists:member(udp, All_opts) of
	       true -> udp_sink(Host, Port);
	       false -> tcp_sink(Host, Port)
	   end,
    Opts = [X || X <- All_opts, X =/= udp],
    new("recorder", Opts, [pcm_source(Span, Timeslot), Sink]).

reset(Name) ->
    tag("reset", [], tag("resource", [{"name", Name}])).

set(Name, Attrs) when is_list(Name), is_list(Attrs) ->
    tag("set", [{"name", Name}],
	    [ attribute(N, stringify(V)) || {N, V} <- Attrs]).

stringify(S) when is_list(S) -> S;
stringify(N) when is_integer(N) -> integer_to_list(N);
stringify({A,B,C,D}) -> tl(lists:append(
			     [["."|integer_to_list(X)] || X <- [A,B,C,D]]));
stringify(B) when is_boolean(B) -> atom_to_list(B).

takeover(Jobs) ->
    tag("takeover", [], [job(Job) || Job <- Jobs]).

tcp_sink(IP, Port) when is_integer(Port) ->
    tag("tcp_sink", [{"ip_addr", IP}, {"ip_port", integer_to_list(Port)}]).

tcp_source(IP, Port) when is_integer(Port) ->
    tag("tcp_source", [{"ip_addr", IP}, {"ip_port", integer_to_list(Port)}]).

tag(Name, Attrs) ->
    tag(Name, Attrs, "").

tag(Name, Attrs, "") ->
    ["<", Name,
     [ [" ", N, "=\"", stringify(V), "\""] || {N, V} <- Attrs],
     "/>"];
tag(Name, Attrs, Child_text) ->
    ["<", Name,
     [ [" ", N, "=\"", stringify(V), "\""] || {N, V} <- Attrs],
     ">", Child_text, "</", Name, ">"].

unmap(Name) ->
    tag("unmap", [{"name", Name}]).

udp_sink(IP, Port) when is_integer(Port) ->
    tag("udp_sink", [{"ip_addr", IP}, {"ip_port", integer_to_list(Port)}]).

wide_recorder(Span, Host, Port, Tag) ->
    new("wide_recorder", [{"span", Span}, {"tag", Tag}],
	[udp_sink(Host, Port)]).


zero_job(Id) ->
    tag("zero", [], tag("job", [{"id", Id}])).

zero_resource(Name) ->
    tag("zero", [], tag("resource", [{"name", Name}])).
