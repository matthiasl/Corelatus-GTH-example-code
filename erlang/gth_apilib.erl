%%%----------------------------------------------------------------------
%%% GTH API: Library code for handling the HTTP-like protocol between
%%%          the GTH and its controllers
%%%
%%% (C) 2001 Corelatus AB Stockholm, Sweden
%%%
%%% This module can be used either with {active, false} or {active, once}
%%% sockets, but some functions only work with one sort. See the 'export'
%%% declaration below.
%%%
%%% In either case, the sockets you pass to this module must have the
%%% 'binary' option set.
%%%
%%% If you're unsure whether you should be using this module, then
%%% you probably don't want to. Take a look at gth.erl instead.
%%%----------------------------------------------------------------------
-module(gth_apilib).
-author("matthias@corelatus.com").

%%% Functions you can ONLY use with 'passive' sockets:
-export([passive_content/2,
	 passive_stream_content/3, passive_stream_content/4,
	 next_non_event/1, next_non_event/2, next_non_event/3]).

%%% Functions you can ONLY use with sockets set up with
%%% [{active, once}, {packet, line}, binary]
-export([active_content/3]).

%%% Functions which don't care which sort of socket is passed in
-export([header/2, send/2, send/3, sendv/2, sendv/3]).

%%% Functions for backwards compatibility
-export([content/2, stream_content/3, stream_content/4]).

%% We have to set an upper limit of something. Larger than this, and it
%% should be streamed.
-define(MAX_CONTENT_SIZE, 500002).

-include("gth_xml.hrl").

%%======================================================================
%% API

%%----------------------------------------------------------------------
%% Reads a blob of content from the socket.
%%
%% Returns:  {Type, Data} | {error, Reason}
%%           Reason: closed | content_too_large | term()
%%           Type: 'text/xml' | 'binary/audio' | 'binary/file'
%%           Data: string() | binary()
%%
%% The Socket must be passive and binary
%%
passive_content(Socket, Timeout) ->
    ok = inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, Line1} ->
	    case gen_tcp:recv(Socket, 0, Timeout) of
		{ok, Line2} ->
		    collect_entire_content(Socket, Line1, Line2, Timeout);
		X ->
		    X
	    end;
	X ->
	    X
    end.

%% Same as content/2, except that the content is passed to the given
%% fun as it arrives. The fun is
%%
%%     fun(Binary) -> ok | {error, Reason}
%%
%% Returns: ok | {error, Reason}
%%  Reason: fun_failed | closed | content_type_mismatch | Other
%%
passive_stream_content(Socket, Timeout, Fun) ->
    passive_stream_content(Socket, "binary/filesystem", Timeout, Fun).
passive_stream_content(Socket, Type, Timeout, Fun) ->
    ok = inet:setopts(Socket, [{packet, line}]),
    case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, Line1} ->
	    case gen_tcp:recv(Socket, 0, Timeout) of
		{ok, Line2} ->
		    stream_entire_content(Socket,
					  list_to_binary(Type ++ "\r\n"),
					  Line1, Line2, Timeout, Fun);
		X ->
		    X
	    end;
	X ->
	    X
    end.


%%----------------------------------------------------------------------
%% Same as 'passive_content', but for 'active' sockets.
%%
%% Line1 is the line received by the caller in active mode
%% Always leaves the socket active for the next message
active_content(Socket, Line1, Timeout) ->
    Return = case gen_tcp:recv(Socket, 0, Timeout) of
		 {ok, Line2} ->
		     collect_entire_content(Socket, Line1, Line2, Timeout);
		 X ->
		     X
	     end,
    ok = inet:setopts(Socket, [{packet, line}, {active, once}]),
    Return.

%% For backwards compatibility.
content(Socket, Timeout) ->
    passive_content(Socket, Timeout).

%% For backwards compatibility.
stream_content(Socket, Timeout, Fun) ->
    passive_stream_content(Socket, "binary/filesystem", Timeout, Fun).

%% For backwards compatibility.
stream_content(Socket, Type, Timeout, Fun) ->
    passive_stream_content(Socket, Type, Timeout, Fun).

%%----------------------------------------------------------------------
%% Given content-type and content, generate the matching header.
%%
%%  Returns: string()
%%

header(Type, Content) when is_binary(Content) ->
    ["Content-type: ", Type, "\r\n",
     "Content-length: ",
     integer_to_list(byte_size(Content)),
     "\r\n\r\n"];

header(Type, Content) when is_list(Content) ->
    ["Content-type: ",
     Type, "\r\n",
     "Content-length: ",
     integer_to_list(lists:flatlength(Content)),
     "\r\n\r\n"];

header(Type, Length) when is_integer(Length) ->
    ["Content-type: ",
     Type, "\r\n",
     "Content-length: ",
     integer_to_list(Length),
     "\r\n\r\n"].


%%----------------------------------------------------------------------
%% Given content, send the whole lot. It's important that there's only
%% one call to gen_tcp (i.e. it's atomic), otherwise concurrent access
%% could cause garbled output, e.g. in connection with events.
%%
%%  Socket: #port
%%  Type: string()
%%  Content: string() | binary()
%%  Returns: ok | {error, Reason}
%%
send(Socket, Type, Content) ->
    gen_tcp:send(Socket, [header(Type, Content), Content]).

%% vector version. expects a list: [{Type, Content}] for arg #2
sendv(Socket, List) ->
    gen_tcp:send(Socket, [ [header(T, C), C] || {T, C} <- List ]).

sendv(Socket, Type, List) ->
    gen_tcp:send(Socket, [ [header(Type, C), C] || C <- List ]).

send(Socket, Content) when is_list(Content) ->
    send(Socket, "text/xml", Content);

send(Socket, Content) when is_binary(Content) ->
    send(Socket, "binary/audio", Content).

%% Discard all events, return the first thing which isn't an event
%%
%% Return: #resp_tuple
%%
%% Only works on passive sockets
next_non_event(Socket) ->
    next_non_event(Socket, 5000).

next_non_event(Socket, Timeout) ->
    next_non_event(Socket, Timeout, false).

next_non_event(Socket, Timeout, Verbose) ->
    case passive_content(Socket, Timeout) of
	{error, Reason} ->
	    exit({socket_problem, Reason});

	{'text/xml', Content} ->
	    S_content = binary_to_list(Content),
	    case gth_client_xml_parse:string(S_content) of
		{ok, Scanned} ->
		    case Scanned#resp_tuple.name of
			event ->
			    case Verbose of
				true -> io:fwrite("~s\n", [S_content]);
				_ -> do_nothing
			    end,
			    next_non_event(Socket, Timeout);
			_ ->
			    Scanned
		    end;
		{error, parse} ->
		    exit({unable_to_parse, S_content})
	    end
    end.


%%======================================================================
%% Internal functions

%% Check the content-type and content-length, return
%%
%%  Return: {Type, Content} | {error, Reason}
%%          Type = atom()
%%          Content = binary()
%%          Reason = content_too_large | atom()
%%
collect_entire_content(S, <<"Content-type: ", Type/binary>>,
		       <<"Content-length: ", BLen/binary>>,
		       Timeout) ->
    Slength = string:substr(binary_to_list(BLen), 1, byte_size(BLen) - 2),
    Length = list_to_integer(Slength) + 2,     %% +2 for crlf
    ok = inet:setopts(S, [{packet, 0}]),

    Atomic_type = case Type of
		      <<"text/xml\r\n">> -> 'text/xml';
		      <<"binary/audio\r\n">> -> 'binary/audio';
		      <<"binary/file\r\n">> -> 'binary/file';
		      <<"text/plain\r\n">> -> 'text/plain';
		      _ -> invalid_content_type
		  end,

    case (Length > ?MAX_CONTENT_SIZE) of
	true ->
	    _ = dump_rest(S, Length, Timeout),
	    {error, content_too_large};
	_ ->
	    case gen_tcp:recv(S, Length, Timeout) of
		{ok, <<_:16, Packet/binary>>}
		when Atomic_type =/= invalid_content_type ->
		    {Atomic_type, Packet};

		{ok, _} ->
		    {error, invalid_content_type};

		{error, Reason} ->
		    {error, Reason}
	    end
    end;

collect_entire_content(_S, _, _, _) ->
    {error, invalid_xml_header}.

%%--------------------
%% Returns ok
%%       | {error, fun_failed}
%%       | {error, content_type_mismatch}
%%       | {error, Reason}
%%
stream_entire_content(S, Expected_type, <<"Content-type: ", Sent_type/binary>>,
		      <<"Content-length: ", Blen/binary>>,
		      Timeout, Supplied_fun) ->
    Slength = string:substr(binary_to_list(Blen), 1, byte_size(Blen) - 2),
    Length = list_to_integer(Slength),
    {ok, _Line} = gen_tcp:recv(S, 0, Timeout),  %% remove leading CRLF
    ok = inet:setopts(S, [{packet, 0}]),

    case Expected_type of
	Sent_type ->  % normal case
	    stream_rest(S, Length, Timeout, Supplied_fun);

	_ ->          % wrong content type. Discard all data.
	    Null_fun = fun(_) -> ok end,
	    stream_rest(S, Length, Timeout, Null_fun),
	    {error, content_type_mismatch}
    end;

%% Bad header. This leaves us in a screwy state which will cause
%% the next valid command to fail. Never mind; if we get here it's
%% because someone did something wrong anyway.
stream_entire_content(_S, _Type, _Typeline, _Length, _Timeout, _Fun) ->
    {error, unexpected_content_type}.

%% Returns ok | {error, Reason}

stream_rest(_, 0, _, Fun) ->    %% nothing left to stream
    case (catch Fun(eof)) of
        ok -> ok;
        Other ->
            error_logger:error_report({"stream fun eof failed", Other}),
            {error, fun_failed}
    end;

stream_rest(S, Bytes, Timeout, Fun) ->
    %% 13216 is an arbitrary number
    Read_now = min(13216, Bytes),
    erlang:garbage_collect(),
    case gen_tcp:recv(S, Read_now, Timeout) of
	{ok, Lump} ->
	    Remaining = Bytes - Read_now,
	    case (catch Fun(Lump)) of
		ok ->
		    stream_rest(S, Remaining, Timeout, Fun);
		Other ->
		    error_logger:error_report({"stream fun failed", Other}),
		    dump_rest(S, Remaining, Timeout)
	    end;
	X = {error, _Reason} ->
	    X
    end.

dump_rest(S, Bytes, Timeout) ->
    Dummy_fun = fun(_) -> ok end,
    case stream_rest(S, Bytes, Timeout, Dummy_fun) of
	ok -> {error, fun_failed};
	X = {error, _Reason} -> X
    end.
