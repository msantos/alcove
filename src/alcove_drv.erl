%%% Copyright (c) 2014-2016, Michael Santos <michael.santos@gmail.com>
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
-export([raw/1, getopts/1, progname/0, port/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type ref() :: pid().
-export_type([ref/0]).

-record(state, {
          owner :: pid(),
          raw = false,
          port :: port(),
          fdctl :: port(),
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
    catch gen_server:call(Drv, stop),
    ok.

-spec call(ref(),[alcove:pid_t()],atom(),list(),timeout()) -> term().
call(Drv, Pids, Command, Argv, Timeout)
    when is_list(Pids), is_atom(Command), is_list(Argv),
         (is_integer(Timeout) orelse Timeout =:= infinity) ->
    Data = alcove_codec:call(Command, Pids, Argv),
    case sync_send(Drv, Data) of
        ok ->
            call_reply(Drv, Pids, alcove_proto:will_return(Command), Timeout);
        Error ->
            Error
    end.

-spec sync_send(ref(),iodata()) -> ok | {alcove_error, badarg}.
sync_send(Drv, Data) ->
    case iolist_size(Data) =< 16#ffff of
        true ->
            gen_server:call(Drv, {send, Data}, infinity);
        false ->
            {alcove_error, badarg}
    end.

-spec send(ref(),iodata()) -> ok | {alcove_error, badarg}.
send(Drv, Data) ->
    case iolist_size(Data) =< 16#ffff of
        true ->
            gen_server:cast(Drv, {send, Data});
        false ->
            {alcove_error, badarg}
    end.

-spec stdin(ref(),[alcove:pid_t()],iodata()) -> 'ok' | {alcove_error, 'badarg'}.
stdin(Drv, Pids, Data) ->
    send(Drv, alcove_codec:stdin(Pids, Data)).

-spec stdout(ref(),[alcove:pid_t()],timeout()) -> 'false' | binary().
stdout(Drv, Pids, Timeout) ->
    reply(Drv, Pids, alcove_stdout, Timeout).

-spec stderr(ref(),[alcove:pid_t()],timeout()) -> 'false' | binary().
stderr(Drv, Pids, Timeout) ->
    reply(Drv, Pids, alcove_stderr, Timeout).

-spec event(ref(),[alcove:pid_t()],timeout()) -> term().
event(Drv, Pids, Timeout) ->
    reply(Drv, Pids, alcove_event, Timeout).

-spec raw(ref()) -> 'ok'.
raw(Drv) ->
    gen_server:call(Drv, raw).

-spec port(ref()) -> port().
port(Drv) ->
    gen_server:call(Drv, port).

%%--------------------------------------------------------------------
%%% Callbacks
%%--------------------------------------------------------------------

% init/2 uses an undocumented feature of open_port/2 to set up a fifo
% between the alcove port process and beam. The close of the fifo indicates
% the port has called exec().
%
% As a result, dialyzer will warn about the first argument passed to
% open_port/2.
-dialyzer({nowarn_function, init/1}).
-dialyzer({no_unused, call_unlink/2}).

init([Owner, Options]) ->
    process_flag(trap_exit, true),

    % Control fifo for the port
    Ctldir = proplists:get_value(ctldir, Options, basedir(?MODULE)),
    Fifo = lists:concat([
            Ctldir,
            "/fdctl.",
            crypto:rand_uniform(0, 16#ffffffff)
        ]),

    [Cmd|Argv] = getopts([{fdctl, Fifo}|Options]),
    PortOpt = lists:filter(fun
            (stderr_to_stdout) -> true;
            ({env,_}) -> true;
            (_) -> false
        end, Options),

    Port = erlang:open_port({spawn_executable, Cmd}, [
            {args, Argv},
            stream,
            binary
        ] ++ PortOpt),

    % Block until the port has fully initialized
    receive
        % {alcove_call, [], ok}
        {Port, {data,<<0,8,0,4,131,100,0,2,111,107>>}} ->
            Fdctl = erlang:open_port(Fifo, [in]),

            % Decrease the link count of the fifo. The fifo is deleted in
            % the port because the port may be running as a different user.
            ok = call_unlink(Port, Fifo),
            {ok, #state{
                    port = Port,
                    fdctl = Fdctl,
                    owner = Owner
                }};
        {'EXIT', Port, normal} ->
            {stop, {error, port_init_failed}};
        {'EXIT', Port, Reason} ->
            {stop, {error, Reason}}
    end.

handle_call({send, Buf}, {Owner,_Tag}, #state{port = Port, owner = Owner} = State) ->
    true = erlang:port_command(Port, Buf),
    {reply, ok, State};

handle_call(raw, {Owner,_Tag}, #state{owner = Owner} = State) ->
    {reply, ok, State#state{raw = true}};

handle_call(port, {Owner,_Tag}, #state{owner = Owner, port = Port} = State) ->
    {reply, Port, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_, {Owner,_Tag}, #state{owner = Owner} = State) ->
    {reply, {error,enotsup}, State};

handle_call(_, _, State) ->
    {reply, {error,not_owner}, State}.

handle_cast({send, Buf}, #state{port = Port} = State) ->
    erlang:port_command(Port, Buf),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{port = Port, fdctl = Fdctl}) ->
    catch erlang:port_close(Port),
    catch erlang:port_close(Fdctl),
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
handle_info({Port, {data, Data}}, #state{raw = true, port = Port, buf = Buf, owner = Owner} = State) ->
    Owner ! {alcove_stdout, self(), [], <<Buf/binary, Data/binary>>},
    {noreply, State};
handle_info({Port, {data, Data}}, #state{port = Port, buf = Buf, owner = Owner} = State) ->
    {Msgs, Rest} = alcove_codec:stream(<<Buf/binary, Data/binary>>),
    Terms = [ alcove_codec:decode(Msg) || Msg <- Msgs ],
    _ = [ Owner ! {Tag, self(), Pids, Term} || {Tag, Pids, Term} <- Terms ],
    {noreply, State#state{buf = Rest}};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {shutdown, Reason}, State};

% The write end of the fdctl fifo has been closed: either the port has
% exit'ed or called exec().
handle_info({'EXIT', Fdctl, _Reason}, #state{fdctl = Fdctl, owner = Owner} = State) ->
    Owner ! {alcove_ctl, self(), [], fdctl_closed},
    {noreply, State#state{raw = true}};

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
        {alcove_ctl, Drv, Pids, Error} ->
            {alcove_error, Error};
        {alcove_call, Drv, Pids, Error} when Error =:= badarg; Error =:= undef ->
            {alcove_error, Error};
        {alcove_call, Drv, Pids, Event} ->
            Event
    after
        Timeout ->
            {alcove_error, timeout}
    end;
call_reply(Drv, Pids, true, Timeout) ->
    receive
        {alcove_ctl, Drv, Pids, fdctl_closed} ->
            call_reply(Drv, Pids, true, Timeout);
        {alcove_ctl, Drv, Pids, Error} ->
            {alcove_error, Error};
        {alcove_event, Drv, Pids, {termsig,_} = Event} ->
            {alcove_error, Event};
        {alcove_event, Drv, Pids, {exit_status,_} = Event} ->
            {alcove_error, Event};
        {alcove_call, Drv, Pids, Error} when Error =:= badarg; Error =:= undef ->
            {alcove_error, Error};
        {alcove_call, Drv, Pids, Event} ->
            Event
    after
        Timeout ->
            {alcove_error, timeout}
    end.

reply(Drv, Pids, Type, Timeout) ->
    receive
        {alcove_ctl, Drv, Pids, fdctl_closed} ->
            reply(Drv, Pids, Type, Timeout);
        {alcove_ctl, Drv, Pids, Error} ->
            {alcove_error, Error};
        {alcove_call, Drv, Pids, Error} when Error =:= badarg; Error =:= undef ->
            {alcove_error, Error};
        {Type, Drv, Pids, Event} ->
            Event
    after
        Timeout ->
            false
    end.

%%--------------------------------------------------------------------
%%% Port executable
%%--------------------------------------------------------------------
-spec getopts(proplists:proplist()) -> list(string() | [string()]).
getopts(Options) when is_list(Options) ->
    Exec = proplists:get_value(exec, Options, ""),
    Progname = proplists:get_value(progname, Options, progname()),

    Options1 = lists:map(fun
                    (N) when is_atom(N) ->
                        {N, true};
                    ({_,_} = N) ->
                        N
                end, Options),

    Switches = lists:append([ optarg(N) || N <- Options1 ]),
    [Cmd|Argv] = [ N || N <- string:tokens(Exec, " ") ++ [Progname|Switches], N /= ""],
    [find_executable(Cmd)|Argv].

optarg({fdctl, Arg})            -> switch("c", Arg);
optarg({maxchild, Arg})         -> switch("m", integer_to_list(Arg));
optarg({depth, Arg})            -> switch("d", integer_to_list(Arg));
optarg(_)                       -> "".

switch(Switch, Arg) when is_binary(Arg) ->
    switch(Switch, binary_to_list(Arg));
switch(Switch, Arg) ->
    [lists:concat(["-", Switch]), Arg].

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

-spec progname() -> string().
progname() ->
    filename:join([basedir(alcove), "alcove"]).

% Blocking functions for handling the Control fd fifo. These functions
% are called from init/1.
call_unlink(Port, File) ->
    Encode = alcove_codec:call(unlink, [], [File]),
    erlang:port_command(Port, Encode),
    Reply = receive
        {Port, {data,Data}} ->
            alcove_codec:decode(Data);
        {'EXIT', Port, normal} ->
            {error, port_init_failed};
        {'EXIT', Port, Reason} ->
            {error, Reason}
    end,
    case Reply of
        {alcove_call, [], ok} ->
            ok;
        {alcove_call, [], Error} ->
            Error;
        {error, _} = Error ->
            Error
    end.
