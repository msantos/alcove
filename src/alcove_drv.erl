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
-export([getopts/1]).

-spec start() -> port().
start() ->
    start([]).

-spec start(proplists:proplist()) -> port().
start(Options) ->
    [Cmd|Argv] = getopts(Options),
    open_port({spawn_executable, Cmd}, [{args, Argv}, {packet, 2}, binary]).

call(Port, Data) ->
    call(Port, Data, infinity).
call(Port, Data, Timeout) when is_port(Port), is_binary(Data), byte_size(Data) < 16#ffff ->
    true = erlang:port_command(Port, Data),
    receive
        {Port, {data, <<?UINT16(?ALCOVE_MSG_CALL), Msg/binary>>}} ->
            binary_to_term(Msg)
    after
        Timeout ->
            {error,timedout}
    end.

cast(Port, Data) when is_port(Port), is_binary(Data), byte_size(Data) < 16#ffff ->
    erlang:port_command(Port, Data).

event(Port) when is_port(Port) ->
    event(Port, 0).

event(Port, Timeout) when is_port(Port) ->
    receive
        {Port, {data, <<?UINT16(Type), Msg/binary>>}} when
            Type =:= ?ALCOVE_MSG_CALL;
            Type =:= ?ALCOVE_MSG_CAST;
            Type =:= ?ALCOVE_MSG_CHILDOUT;
            Type =:= ?ALCOVE_MSG_CHILDERR ->
                {type_to_atom(Type), binary_to_term(Msg)}
    after
        Timeout ->
            false
    end.

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

    Expand = lists:map(fun
                    (verbose) -> {verbose, 1};
                    (N) when is_atom(N) -> {N, true};
                    ({_,_} = N) -> N
                end, Options),

    Opt = lists:ukeysort(1, Expand),

    Switches = lists:append([ optarg(N) || N <- Opt ]),
    [Cmd|Argv] = [ N || N <- string:tokens(Exec, " ") ++ [Progname|Switches], N /= ""],
    [find_executable(Cmd)|Argv].

optarg({verbose, Arg})          -> switch(string:copies("v", Arg));
optarg(_)                       -> "".

switch(Switch) ->
    [lists:concat(["-", Switch])].

%switch(Switch, Arg) when is_binary(Arg) ->
%    switch(Switch, binary_to_list(Arg));
%switch(Switch, Arg) ->
%    [lists:concat(["-", Switch]), Arg].

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
