%%% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-module(alcove_drv).
-behaviour(gen_server).
-include_lib("alcove/include/alcove.hrl").

%% API
-export([start/0, start/1, stop/1]).
-export([start_link/2]).
-export([call/2, call/3, call/4, cast/2, encode/2, encode/3]).
-export([stdin/3, stdout/3, stderr/3, event/3]).
-export([atom_to_type/1, type_to_atom/1]).
-export([msg/2, decode/1]).
-export([getopts/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        pid :: pid(),
        port :: port()
    }).

-spec start() -> {ok, pid()}.
start() ->
    start_link(self(), []).

-spec start(proplists:proplist()) -> {ok, pid()}.
start(Options) ->
    start_link(self(), Options).

-spec start_link(pid(), proplists:proplist()) -> {ok, pid()}.
start_link(Owner, Options) ->
    gen_server:start_link(?MODULE, [Owner, Options], []).

-spec stop(pid()) -> ok.
stop(Drv) ->
    gen_server:call(Drv, stop).

-spec call(pid(),iodata()) -> any().
call(Drv, Data) ->
    call(Drv, [], Data, 5000).

-spec call(pid(),[integer()],iodata()) -> any().
call(Drv, Pids, Data) ->
    call(Drv, Pids, Data, 5000).

-spec call(pid(),[integer()],iodata(),'infinity' | non_neg_integer()) ->
    any().
call(Drv, Pids, Data, Timeout) ->
    true = send(Drv, Data),
    reply(Drv, Pids, ?ALCOVE_MSG_CALL, Timeout).

-spec cast(pid(),iodata()) -> any().
cast(Drv, Data) ->
    send(Drv, Data).

-spec send(pid(),iodata()) -> any().
send(Drv, Data) ->
    gen_server:call(Drv, {send, Data}, infinity).

-spec stdin(pid(),[integer()],iodata()) -> 'true'.
stdin(Drv, [], Data) ->
    cast(Drv, Data);
stdin(Drv, Pids, Data) ->
    Stdin = hdr(lists:reverse(Pids), [Data]),
    cast(Drv, Stdin).

-spec stdout(pid(),[integer()],'infinity' | non_neg_integer()) ->
    'false' | binary().
stdout(Drv, Pids, Timeout) ->
    reply(Drv, Pids, ?ALCOVE_MSG_STDOUT, Timeout).

-spec stderr(pid(),[integer()],'infinity' | non_neg_integer()) ->
    'false' | binary().
stderr(Drv, Pids, Timeout) ->
    reply(Drv, Pids, ?ALCOVE_MSG_STDERR, Timeout).

-spec event(pid(),[integer()], 'infinity' | non_neg_integer()) -> any().
event(Drv, Pids, Timeout) ->
    reply(Drv, Pids, ?ALCOVE_MSG_EVENT, Timeout).

msg([], Data) ->
    Data;
msg(Pids, Data) ->
    Size = iolist_size(Data),
    hdr(lists:reverse(Pids), [<<?UINT16(Size)>>, Data]).

hdr([], [_Length|Acc]) ->
    Acc;
hdr([Pid|Pids], Acc) ->
    Size = iolist_size(Acc) + 2 + 4,
    hdr(Pids, [<<?UINT16(Size)>>, <<?UINT16(?ALCOVE_MSG_STDIN)>>, <<?UINT32(Pid)>>|Acc]).

encode(Command, Arg) when is_integer(Command), is_list(Arg) ->
    encode(?ALCOVE_MSG_CALL, Command, Arg).
encode(Type, Command, Arg) when is_integer(Type), is_integer(Command), is_list(Arg) ->
    <<?UINT16(Type), ?UINT16(Command), (term_to_binary(Arg))/binary>>.

decode(Msg) ->
    % Re-add the message length stripped off by open_port
    Len = byte_size(Msg),
    decode(<<?UINT16(Len), Msg/binary>>, [], [], []).

decode(<<>>, _Pids, Msg, Acc) ->
    lists:flatten(lists:reverse([Msg|Acc]));

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_PROXY), ?UINT32(Pid),
    Rest/binary>>, Pids, [], Acc) when Len =:= 2 + 4 + byte_size(Rest) ->
    decode(Rest, [Pid|Pids], [], Acc);
decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_PROXY), ?UINT32(Pid),
    Rest/binary>>, _Pids, Msg, Acc) when Len =:= 2 + 4 + byte_size(Rest) ->
    decode(Rest, [Pid], [], [lists:reverse(Msg)|Acc]);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_STDOUT), ?UINT32(Pid),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - (2 + 4),
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_stdout, lists:reverse([Pid|Pids]), Reply}|Msg], Acc);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_STDERR), ?UINT32(Pid),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - (2 + 4),
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_stderr, lists:reverse([Pid|Pids]), Reply}|Msg], Acc);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_CALL),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - 2,
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_call, lists:reverse(Pids), binary_to_term(Reply)}|Msg],
        Acc);

decode(<<?UINT16(Len), ?UINT16(?ALCOVE_MSG_EVENT),
    Bin/binary>>, Pids, Msg, Acc) ->
    Bytes = Len - 2,
    <<Reply:Bytes/bytes, Rest/binary>> = Bin,
    decode(Rest, Pids,
        [{alcove_event, lists:reverse(Pids), binary_to_term(Reply)}|Msg],
        Acc).

%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------
init([Owner, Options]) ->
    process_flag(trap_exit, true),

    [Cmd|Argv] = getopts(Options),
    PortOpt = lists:filter(fun
            (stderr_to_stdout) -> true;
            ({env,_}) -> true;
            (_) -> false
        end, Options),

    Port = open_port({spawn_executable, Cmd}, [
            {args, Argv},
            {packet, 2},
            binary
        ] ++ PortOpt),

    {ok, #state{port = Port, pid = Owner}}.

handle_call({send, Packet}, _From, #state{port = Port} = State) ->
    Reply = try erlang:port_command(Port, Packet) of
        true ->
            true
        catch
            error:badarg ->
                {error,closed}
        end,
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    catch erlang:port_close(Port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Port communication
%%--------------------------------------------------------------------

% Reply from a child process.
%
% The parent process may coalesce 2 writes from the child into 1 read. The
% parent could read the length header then read length bytes except that
% the child may have called execvp(). After calling exec(), the data
% returned from the child will not contain a length header.
%
% Work around this by converting the reply into a list of messages. The
% first message matching the requested type is returned to the caller. The
% remaining messages are pushed back into the process' mailbox.
handle_info({Port, {data, Data}}, #state{port = Port, pid = Pid} = State) ->
    [ Pid ! {self(), Msg} || Msg <- decode(Data) ],
    {noreply, State};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {shutdown, Reason}, State};

% WTF
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
reply(Drv, Pids, Type, Timeout) ->
    Tag = type_to_atom(Type),
    receive
        {Drv, {Tag, Pids, Event}} ->
            Event
    after
        Timeout ->
            false
    end.

-spec getopts(proplists:proplist()) -> list(string() | [string()]).
getopts(Options) when is_list(Options) ->
    Exec = proplists:get_value(exec, Options, ""),
    Progname = proplists:get_value(progname, Options, progname()),

    Options1 = lists:map(fun
                    (verbose) ->
                        {verbose, 1};
                    (N) when is_atom(N) ->
                        {N, true};
                    ({_,_} = N) ->
                        N
                end, Options),

    Switches = lists:append([ optarg(N) || N <- Options1 ]),
    [Cmd|Argv] = [ N || N <- string:tokens(Exec, " ") ++ [Progname|Switches], N /= ""],
    [find_executable(Cmd)|Argv].

optarg({exit_status, Bool})     -> switch("e", bool(Bool));
optarg({sigchld, Bool})         -> switch("s", bool(Bool));
optarg({termsig, Bool})         -> switch("S", bool(Bool));
optarg({verbose, Arg})          -> switch(string:copies("v", Arg));
optarg({maxchild, Arg})         -> switch("m", Arg);
optarg({maxforkdepth, Arg})     -> switch("M", Arg);
optarg(_)                       -> "".

switch(Switch) ->
    [lists:concat(["-", Switch])].

switch(Switch, Arg) when is_binary(Arg) ->
    switch(Switch, binary_to_list(Arg));
switch(Switch, Arg) ->
    [lists:concat(["-", Switch, " ", Arg])].

bool(true) -> 1;
bool(false) -> 0.

find_executable(Exe) ->
    case os:find_executable(Exe) of
        false ->
            erlang:error(badarg, [Exe]);
        N ->
            N
    end.

atom_to_type(alcove_call) -> ?ALCOVE_MSG_CALL;
atom_to_type(alcove_event) -> ?ALCOVE_MSG_EVENT;
atom_to_type(alcove_stdin) -> ?ALCOVE_MSG_STDIN;
atom_to_type(alcove_stdout) -> ?ALCOVE_MSG_STDOUT;
atom_to_type(alcove_stderr) -> ?ALCOVE_MSG_STDERR;
atom_to_type(alcove_proxy) -> ?ALCOVE_MSG_PROXY.

type_to_atom(?ALCOVE_MSG_CALL) -> alcove_call;
type_to_atom(?ALCOVE_MSG_EVENT) -> alcove_event;
type_to_atom(?ALCOVE_MSG_STDIN) -> alcove_stdin;
type_to_atom(?ALCOVE_MSG_STDOUT) -> alcove_stdout;
type_to_atom(?ALCOVE_MSG_STDERR) -> alcove_stderr;
type_to_atom(?ALCOVE_MSG_PROXY) -> alcove_proxy.

basedir(Module) ->
    case code:priv_dir(Module) of
        {error, bad_name} ->
            filename:join([
                filename:dirname(code:which(Module)),
                "..",
                "priv"
            ]);
        Dir ->
            Dir
        end.

progname() ->
    filename:join([basedir(alcove), "alcove"]).
