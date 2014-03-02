#!/usr/bin/env escript

%%%
%%% Generate the alcove.erl file
%%%
main([]) ->
    File = "liblxc.erl",
    Proto = "c_src/alcove_cmd.proto",
    main([File, Proto]);

main([File, Proto]) ->
    mkerl(File, Proto).

license() ->
    {{Year,_,_},{_,_,_}} = calendar:universal_time(),

    Date = integer_to_list(Year),

    License = [
" Copyright (c) " ++ Date ++ ", Michael Santos <michael.santos@gmail.com>",
" Permission to use, copy, modify, and/or distribute this software for any",
" purpose with or without fee is hereby granted, provided that the above",
" copyright notice and this permission notice appear in all copies.",
"",
" THE SOFTWARE IS PROVIDED \"AS IS\" AND THE AUTHOR DISCLAIMS ALL WARRANTIES",
" WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF",
" MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR",
" ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES",
" WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN",
" ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF",
" OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE."],

    erl_syntax:comment(License).

api(Proto) ->
    Calls = calls(Proto),


    % Generate the function
    Pattern = [],
    Body = erl_syntax:tuple([ erl_syntax:atom(N) || {N,_} <- Calls ]),
    Clause = erl_syntax:clause(Pattern, [], [Body]),
    [erl_syntax:function(erl_syntax:atom("api"), [Clause])].

mkerl(File, Proto) ->
    Module = erl_syntax:attribute(
            erl_syntax:atom(module),
            [erl_syntax:atom(filename:basename(File, ".erl"))]
            ),
    Includes = includes(["alcove.hrl"]),

    % Type specs
    Specs = erl_syntax:comment(["%__SPECS__%%"]),

    % Any hardcoded functions will be included here
    Static = erl_syntax:comment(["%__STATIC__%%"]),

    Calls = calls(Proto),

    % Generate the list of exports
    Comment_static = erl_syntax:comment([" Static functions"]),
    Exports_static = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity))
                        || {Fun, Arity} <- static_exports() ])
                ]),

    Comment_gen = erl_syntax:comment([" Generated functions"]),
    Exports_gen = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity+1))
                        || {Fun, Arity} <- Calls ])
                ]),

    % Generate the functions
    Functions = [ begin
                    % name(Port, ...) -> liblxc:call(Port, Fun, [...])
                    Arg = arg("Arg", Arity),
                    Pattern = [erl_syntax:variable("Port")|Arg],
                    Body = erl_syntax:application(
                        erl_syntax:atom(call),
                        [erl_syntax:variable("Port"), erl_syntax:atom(Fun),
                            erl_syntax:list(Arg)]
                    ),
                    Clause = erl_syntax:clause(Pattern, [], [Body]),
                    erl_syntax:function(erl_syntax:atom(Fun), [Clause])
                end || {Fun, Arity} <- Calls ],

    Code0 = erl_prettypr:format(erl_syntax:form_list(lists:flatten([
                license(),
                Module,
                Includes,

                Specs,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen,

                Static,
                api(Proto),
                Functions
            ]))),

    Code = lists:foldl(fun({Marker, Generated}, Text) ->
                re:replace(Text, Marker, Generated)
        end,
        Code0,
        [
            {"%%__STATIC__%%", static()},
            {"%%__SPECS__%%", specs()}
        ]),

%    io:format("~s~n", [Code]).
    file:write_file(File, [Code]).

arg(Prefix, Arity) ->
    [ erl_syntax:variable(string:concat(Prefix, integer_to_list(N))) || N <- lists:seq(1,Arity) ].

% List the supported liblxc API functions
calls(Proto) ->
    {ok, Bin} = file:read_file(Proto),
    Fun = binary:split(Bin, <<"\n">>, [trim,global]),
    call_to_fun(Fun, []).

call_to_fun([], Acc) ->
    lists:reverse(Acc);
call_to_fun([H|T], Acc) ->
    [Fun, Arity] = binary:split(H, <<"/">>),
    Name = case Fun of
        <<"lxc_container_", Rest/binary>> ->
            Rest;
        _ ->
            Fun
    end,
    call_to_fun(T, [{binary_to_list(Name), binary_to_integer(Arity)}|Acc]).

static_exports() ->
    [{stdin,2},
     {stdout,1}, {stdout,2},
     {stderr,1}, {stderr,2},
     {recv,1}, {recv,2},
     {setrlimit,3},
     {command,1},
     {call,2},
     {call,3}].

static() ->
    [ static({Fun, Arity}) || {Fun, Arity} <- static_exports() ].

static({stdin,2}) ->
"
stdin(Port, Data) ->
    alcove_drv:cast(Port, <<?UINT16(?ALCOVE_MSG_CHILDIN), Data/binary>>).
";

static({stdout,1}) ->
"
stdout(Port) ->
    stdout(Port, 0).
";
static({stdout,2}) ->
"
stdout(Port, Timeout) ->
    receive
        {Port, {data, <<?UINT16(?ALCOVE_MSG_CHILDOUT), Msg/binary>>}} ->
            binary_to_term(Msg)
    after
        Timeout ->
            false
    end.
";

static({stderr,1}) ->
"
stderr(Port) ->
    stderr(Port, 0).
";
static({stderr,2}) ->
"
stderr(Port, Timeout) ->
    receive
        {Port, {data, <<?UINT16(?ALCOVE_MSG_CHILDERR), Msg/binary>>}} ->
            binary_to_term(Msg)
    after
        Timeout ->
            false
    end.
";

static({recv,1}) ->
"
recv(Port) ->
    recv(Port, 0).
";
static({recv,2}) ->
"
recv(Port, Timeout) ->
    receive
        {Port, {data, <<?UINT16(?ALCOVE_MSG_CALL), Msg/binary>>}} ->
            binary_to_term(Msg)
    after
        Timeout ->
            false
    end.
";

static({setrlimit,3}) ->
"
setrlimit(Port, Resource, #rlimit{cur = Cur, max = Max}) ->
    setrlimit(Port, Resource, Cur, Max).
";

static({command,1}) ->
"
command(Cmd) when is_atom(Cmd) ->
    lookup(Cmd, api()).

lookup(Cmd, Cmds) ->
    lookup(Cmd, 1, Cmds, tuple_size(Cmds)).
lookup(Cmd, N, Cmds, _Max) when Cmd =:= element(N, Cmds) ->
    % Convert to 0 offset
    N-1;
lookup(Cmd, N, Cmds, Max) when N =< Max ->
    lookup(Cmd, N+1, Cmds, Max).
";
static({call,2}) ->
"
call(Port, Command) ->
    call(Port, Command, []).
";
static({call,3}) ->
"
call(Port, execvp, Arg) when is_port(Port), is_list(Arg) ->
    alcove_drv:cast(Port, alcove_drv:encode(command(execvp), Arg));
call(Port, Command, Arg) when is_port(Port), is_list(Arg) ->
    case alcove_drv:call(Port, alcove_drv:encode(command(Command), Arg)) of
        badarg ->
            erlang:error(badarg, [Port, Command, Arg]);
        Reply ->
            Reply
    end.
".

includes(Header) ->
    [ erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header ].

% FIXME hack for hard coding typespecs
specs() ->
"".
