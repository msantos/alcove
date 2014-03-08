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
-include_lib("alcove/include/alcove.hrl").

%% API
-export([start/0, start/1, stop/1]).
-export([call/2, call/3, cast/2, encode/2, encode/3, event/1, event/2]).
-export([msg/2, msg/3]).
-export([getopts/1]).

-spec start() -> port().
start() ->
    start([]).

-spec start(proplists:proplist()) -> port().
start(Options) ->
    [Cmd|Argv] = getopts(Options),
    open_port({spawn_executable, Cmd}, [{args, Argv}, {packet, 2}, binary]).

-spec call(port(),iodata()) -> 'badarg' | boolean() | binary() | non_neg_integer() | [integer()] | 'ok' | {'ok', binary() | non_neg_integer() | #rlimit{}} | {'error', file:posix()}.
call(Port, Data) ->
    call(Port, Data, 5000).

-spec call(port(),iodata(),'infinity' | non_neg_integer()) -> any().
call(Port, Data, Timeout) ->
    true = send(Port, Data, iolist_size(Data)),
    receive
        {Port, {data, <<?UINT16(?ALCOVE_MSG_CALL), Reply/binary>>}} ->
            binary_to_term(Reply);
        % XXX ignore the PID
        {Port, {data, <<?UINT16(?ALCOVE_MSG_CHILDOUT), ?UINT32(_Pid), ?UINT16(Len), ?UINT16(?ALCOVE_MSG_CALL), Reply/binary>>}} when Len =:= 2 + byte_size(Reply) ->
            binary_to_term(Reply)
    after
        Timeout ->
            {error,timedout}
    end.

-spec cast(port(),iodata()) -> any().
cast(Port, Data) ->
    send(Port, Data, iolist_size(Data)).

-spec send(port(),iodata(),pos_integer()) -> any().
send(Port, Data, Size) when is_port(Port), Size < 16#ffff ->
    erlang:port_command(Port, Data).

event(Port) when is_port(Port) ->
    event(Port, 0).

event(Port, Timeout) when is_port(Port) ->
    receive
        {Port, {data, <<?UINT16(Type), Reply/binary>>}} when
            Type =:= ?ALCOVE_MSG_CALL;
            Type =:= ?ALCOVE_MSG_CAST;
            Type =:= ?ALCOVE_MSG_CHILDOUT;
            Type =:= ?ALCOVE_MSG_CHILDERR ->
                {type_to_atom(Type), binary_to_term(Reply)}
    after
        Timeout ->
            false
    end.

msg([], Data) ->
    Data;
msg(Pids, Data) ->
    Size = iolist_size(Data),
    msg(lists:reverse(Pids), Data, [<<?UINT16(Size)>>, Data]).

msg([], _Data, [_Length|Acc]) ->
    Acc;
msg([Pid|Pids], Data, Acc) ->
    Size = iolist_size(Acc) + 2 + 4,
    msg(Pids, Data, [<<?UINT16(Size)>>, <<?UINT16(?ALCOVE_MSG_CHILDIN)>>, <<?UINT32(Pid)>>|Acc]).

encode(Command, Arg) when is_integer(Command), is_list(Arg) ->
    encode(?ALCOVE_MSG_CALL, Command, Arg).
encode(Type, Command, Arg) when is_integer(Type), is_integer(Command), is_list(Arg) ->
    <<?UINT16(Type), ?UINT16(Command), (term_to_binary(Arg))/binary>>.

stop(Port) when is_port(Port) ->
    erlang:port_close(Port).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec getopts(proplists:proplist()) -> list(string() | [string()]).
getopts(Options) when is_list(Options) ->
    Exec = proplists:get_value(exec, Options, "sudo"),
    Progname = proplists:get_value(progname, Options, progname()),

    Options1 = proplists:substitute_aliases([{ns, namespace}], Options),

    Options2 = lists:map(fun
                    (verbose) ->
                        {verbose, 1};
                    ({Tag, [H|_] = N}) when is_atom(Tag), (is_list(H) orelse is_binary(H)) ->
                        [{Tag, X} || X <- N];
                    (N) when is_atom(N) ->
                        {N, true};
                    ({_,_} = N) ->
                        N
                end, Options1),

    Switches = lists:append([ optarg(N) || N <- lists:flatten(Options2) ]),
    [Cmd|Argv] = [ N || N <- string:tokens(Exec, " ") ++ [Progname|Switches], N /= ""],
    [find_executable(Cmd)|Argv].

optarg({verbose, Arg})          -> switch(string:copies("v", Arg));
optarg({namespace, Arg})        -> switch("n", Arg);
optarg(_)                       -> "".

switch(Switch) ->
    [lists:concat(["-", Switch])].

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

type_to_atom(?ALCOVE_MSG_CALL) -> call;
type_to_atom(?ALCOVE_MSG_CAST) -> cast;
%type_to_atom(?ALCOVE_MSG_CHILDIN) -> stdin;
type_to_atom(?ALCOVE_MSG_CHILDOUT) -> stdout;
type_to_atom(?ALCOVE_MSG_CHILDERR) -> stderr.

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
