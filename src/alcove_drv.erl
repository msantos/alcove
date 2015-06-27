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
-export([start/0, start/1, start/2, stop/1]).
-export([start_link/0, start_link/1, start_link/2]).
-export([call/5]).
-export([stdin/3, stdout/3, stderr/3, event/3]).
-export([getopts/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type ref() :: pid().
-export_type([ref/0]).

-record(state, {
        ps,
        port :: port(),
        buf = <<>> :: binary()
    }).

-spec start() -> {ok, ref()}.
start() ->
    start(self(), []).

-spec start(proplists:proplist()) -> {ok, ref()}.
start(Options) ->
    start(self(), Options).

-spec start(pid(), proplists:proplist()) -> {ok, ref()}.
start(Owner, Options) ->
    gen_server:start(?MODULE, [Owner, Options], []).

-spec start_link() -> {ok, ref()}.
start_link() ->
    start_link(self(), []).

-spec start_link(proplists:proplist()) -> {ok, ref()}.
start_link(Options) ->
    start_link(self(), Options).

-spec start_link(pid(), proplists:proplist()) -> {ok, ref()}.
start_link(Owner, Options) ->
    gen_server:start_link(?MODULE, [Owner, Options], []).

-spec stop(ref()) -> ok.
stop(Drv) ->
    gen_server:call(Drv, stop).

-spec call(ref(),alcove:fork_path(),atom(),list(),timeout()) -> term().
call(Drv, Pids, Command, Argv, Timeout)
    when is_list(Pids), is_atom(Command), is_list(Argv),
         (is_integer(Timeout) orelse Timeout =:= infinity) ->
    Data = alcove_codec:call(Command, Pids, Argv),
    case sync_send(Drv, Pids, Data) of
        true ->
            call_reply(Drv, Pids, alcove_proto:will_return(Command), Timeout);
        Error ->
            Error
    end.

-spec sync_send(ref(),alcove:fork_path(),iodata()) -> true | badarg.
sync_send(Drv, Pids, Data) ->
    case iolist_size(Data) =< 16#ffff of
        true ->
            gen_server:call(Drv, {send, Pids, Data}, infinity);
        false ->
            badarg
    end.

-spec send(ref(),alcove:fork_path(),iodata()) -> true | badarg.
send(Drv, Pids, Data) ->
    case iolist_size(Data) =< 16#ffff of
        true ->
            gen_server:cast(Drv, {send, Pids, Data}),
            true;
        false ->
            badarg
    end.

-spec stdin(ref(),alcove:fork_path(),iodata()) -> 'true'.
stdin(Drv, Pids, Data) ->
    Stdin = alcove_codec:stdin(Pids, Data),
    send(Drv, Pids, Stdin).

-spec stdout(ref(),alcove:fork_path(),timeout()) -> 'false' | binary().
stdout(Drv, Pids, Timeout) ->
    reply(Drv, Pids, alcove_stdout, Timeout).

-spec stderr(ref(),alcove:fork_path(),timeout()) -> 'false' | binary().
stderr(Drv, Pids, Timeout) ->
    reply(Drv, Pids, alcove_stderr, Timeout).

-spec event(ref(),alcove:fork_path(),timeout()) -> term().
event(Drv, Pids, Timeout) ->
    reply(Drv, Pids, alcove_event, Timeout).

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
            stream,
            binary
        ] ++ PortOpt),

    {ok, #state{port = Port, ps = dict:store([], Owner, dict:new())}}.

handle_call({send, ForkPath, Packet}, {Pid,_Tag}, #state{port = Port, ps = PS} = State) ->
    case is_monitored(Pid) of
        true -> ok;
        false -> monitor(process, Pid)
    end,
    Reply = erlang:port_command(Port, Packet),
    {reply, Reply, State#state{ps = dict:store(ForkPath, Pid, PS)}};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({send, _ForkPath, Packet}, #state{port = Port} = State) ->
    erlang:port_command(Port, Packet),
    {noreply, State};
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
% Several writes from the child process may be coalesced into 1 read by
% the parent.
handle_info({Port, {data, Data}}, #state{port = Port, buf = Buf, ps = PS} = State) ->
    {Msgs, Rest} = alcove_codec:stream(<<Buf/binary, Data/binary>>),
    Terms = [ alcove_codec:decode(Msg) || Msg <- Msgs ],
    Pid = dict:fetch([], PS),
    [ get_value(Pids, PS, Pid) ! {Tag, self(), Pids, Term}
        || {Tag, Pids, Term} <- Terms ],
    {noreply, State#state{buf = Rest}};

handle_info({'DOWN', _MonitorRef, _Type, Pid, _Info}, #state{ps = PS} = State) ->
    {noreply, State#state{
            ps = dict:filter(fun(_K,V) -> V =/= Pid end, PS)
        }};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {shutdown, Reason}, State};

% WTF
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call_reply(Drv, Pids, false, Timeout) ->
    receive
        {alcove_ctl, Drv, Pids, fdctl_closed} ->
            ok;
        {alcove_call, Drv, _Pids, badpid} ->
            exit(badpid);
        {alcove_call, Drv, Pids, Event} ->
            Event
    after
        Timeout ->
            exit(timeout)
    end;
call_reply(Drv, Pids, true, Timeout) ->
    receive
        {alcove_ctl, Drv, Pids, fdctl_closed} ->
            call_reply(Drv, Pids, true, Timeout);
        {alcove_event, Drv, Pids, {termsig,_} = Event} ->
            exit(Event);
        {alcove_event, Drv, Pids, {exit_status,_} = Event} ->
            exit(Event);
        {alcove_call, Drv, _Pids, badpid} ->
            exit(badpid);
        {alcove_call, Drv, Pids, Event} ->
            Event
    after
        Timeout ->
            exit(timeout)
    end.

reply(Drv, Pids, Type, Timeout) ->
    receive
        {alcove_call, Drv, _Pids, badpid} ->
            exit(badpid);
        {Type, Drv, Pids, Event} ->
            Event
    after
        Timeout ->
            false
    end.

is_monitored(Pid) ->
    {monitored_by, Monitors} = process_info(Pid, monitored_by),
    lists:member(self(), Monitors).

get_value(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        error -> Default;
        {ok,Val} -> Val
    end.

%%--------------------------------------------------------------------
%%% Port executable
%%--------------------------------------------------------------------
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

optarg({verbose, Arg})          -> switch(string:copies("v", Arg));
optarg({maxchild, Arg})         -> switch("m", Arg);
optarg(_)                       -> "".

switch(Switch) ->
    [lists:concat(["-", Switch])].

switch(Switch, Arg) when is_binary(Arg) ->
    switch(Switch, binary_to_list(Arg));
switch(Switch, Arg) ->
    [lists:concat(["-", Switch, " ", Arg])].

find_executable(Exe) ->
    case os:find_executable(Exe) of
        false ->
            erlang:error(badarg, [Exe]);
        N ->
            N
    end.

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
