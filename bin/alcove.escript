#!/usr/bin/env escript

%%%
%%% Generate the alcove.erl file
%%%
main([]) ->
    File = "alcove.erl",
    Proto = "c_src/alcove_call.proto",
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
    Exports_gen0 = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity+1))
                        || {Fun, Arity} <- Calls ])
                ]),

    Exports_gen1 = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity+2))
                        || {Fun, Arity} <- Calls ])
                ]),

    % Generate the functions
    Functions = [ begin
                    % name(Port, ...) -> alcove:call(Port, [], Fun, [...])
                    Arg = arg("Arg", Arity),

                    Pattern0 = [erl_syntax:variable("Port")|Arg],
                    Body0 = erl_syntax:application(
                        erl_syntax:atom(call),
                        [erl_syntax:variable("Port"), erl_syntax:nil(),
                            erl_syntax:atom(Fun), erl_syntax:list(Arg)]
                    ),
                    Clause0 = erl_syntax:clause(Pattern0, [], [Body0]),

                    % name(Port, Pids, ...) -> alcove:call(Port, Pids, Fun, [...])
                    Pattern1 = [erl_syntax:variable("Port"), erl_syntax:variable("Pids")|Arg],
                    Body1 = erl_syntax:application(
                        erl_syntax:atom(call),
                        [erl_syntax:variable("Port"), erl_syntax:variable("Pids"),
                            erl_syntax:atom(Fun), erl_syntax:list(Arg)]
                    ),
                    Clause1 = erl_syntax:clause(Pattern1, [], [Body1]),

                    [erl_syntax:function(erl_syntax:atom(Fun), [Clause0]),
                        erl_syntax:function(erl_syntax:atom(Fun), [Clause1])]

                end || {Fun, Arity} <- Calls ],

    Code0 = erl_prettypr:format(erl_syntax:form_list(lists:flatten([
                license(),
                Module,
                Includes,

                Specs,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen0,
                Exports_gen1,

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

% List the supported alcove API functions
calls(Proto) ->
    {ok, Bin} = file:read_file(Proto),
    Fun = binary:split(Bin, <<"\n">>, [trim,global]),
    call_to_fun(Fun, []).

call_to_fun([], Acc) ->
    lists:reverse(Acc);
call_to_fun([H|T], Acc) ->
    [Fun, Arity] = binary:split(H, <<"/">>),
    call_to_fun(T, [{binary_to_list(Fun), b2i(Arity)}|Acc]).

b2i(N) when is_binary(N) ->
    list_to_integer(binary_to_list(N)).

static_exports() ->
    [{define,2},
     {stdin,2}, {stdin,3},
     {stdout,1}, {stdout,2}, {stdout,3},
     {stderr,1}, {stderr,2}, {stderr,3},
     {event,1}, {event,2}, {event,3},
     {encode,3},
     {command,1},
     {call,2},
     {call,3},
     {call,4}].

static() ->
    [ static({Fun, Arity}) || {Fun, Arity} <- static_exports() ].

static({define,2}) ->
"
define(Port, Const) when is_atom(Const) ->
    define(Port, [Const]);
define(Port, Consts) when is_list(Consts) ->
    lists:foldl(fun(Const,A) ->
                N = case atom_to_list(Const) of
                    \"CLONE_\" ++ _ ->
                        alcove:clone_define(Port, Const);
                    \"MS_\" ++ _ ->
                        alcove:mount_define(Port, Const);
                    \"MNT_\" ++ _ ->
                        alcove:mount_define(Port, Const);
                    \"O_\" ++ _ ->
                        alcove:file_define(Port, Const);
                    \"PR_\" ++ _ ->
                        alcove:prctl_define(Port, Const);
                    \"RLIMIT_\" ++ _ ->
                        alcove:rlimit_define(Port, Const);
                    \"SIG\" ++ _ ->
                        alcove:signal_define(Port, Const);
                    Flag when
                        Flag =:= \"rdonly\";
                        Flag =:= \"nosuid\";
                        Flag =:= \"noexec\";
                        Flag =:= \"noatime\" ->
                            alcove:mount_define(Port, Const)
                end,
                N bxor A
        end,
        0,
        Consts).
";

static({stdin,2}) ->
"
stdin(Port, Data) ->
    stdin(Port, [], Data).
";
static({stdin,3}) ->
"
stdin(Port, Pids, Data) ->
    alcove_drv:stdin(Port, Pids, Data).
";

static({stdout,1}) ->
"
stdout(Port) ->
    stdout(Port, [], 0).
";
static({stdout,2}) ->
"
stdout(Port, Pids) ->
    stdout(Port, Pids, 0).
";
static({stdout,3}) ->
"
stdout(Port, Pids, Timeout) ->
    alcove_drv:stdout(Port, Pids, Timeout).
";

static({stderr,1}) ->
"
stderr(Port) ->
    stderr(Port, [], 0).
";
static({stderr,2}) ->
"
stderr(Port, Pids) ->
    stderr(Port, Pids, 0).
";
static({stderr,3}) ->
"
stderr(Port, Pids, Timeout) ->
    alcove_drv:stderr(Port, Pids, Timeout).
";

static({event,1}) ->
"
event(Port) ->
    event(Port, [], 0).
";
static({event,2}) ->
"
event(Port, Pids) ->
    event(Port, Pids, 0).
";
static({event,3}) ->
"
event(Port, Pids, Timeout) ->
    alcove_drv:event_data(Port, Pids, ?ALCOVE_MSG_EVENT, Timeout).
";

static({encode,3}) ->
"
encode(Call, Pids, Arg) when is_atom(Call), is_list(Pids), is_list(Arg) ->
    Bin = alcove_drv:encode(command(Call), Arg),
    alcove_drv:msg(Pids, Bin).
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
    call(Port, [], Command, []).
";
static({call,3}) ->
"
call(Port, Command, Options) ->
    call(Port, [], Command, Options).
";
static({call,4}) ->
"
call(Port, Pids, Command, Arg) when is_port(Port), is_list(Arg) ->
    case alcove_drv:call(Port, Pids, encode(Command, Pids, Arg)) of
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
"
-spec chdir(port(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chdir(port(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec chroot(port(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chroot(port(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec clone(port(),non_neg_integer()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.
-spec clone(port(),[integer()],non_neg_integer()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.

-spec clone_define(port(),atom()) -> 'false' | non_neg_integer().
-spec clone_define(port(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec define(port(),atom() | [atom()]) -> integer().

-spec execvp(port(),iodata(),iodata()) -> 'ok'.
-spec execvp(port(),list(integer()),iodata(),iodata()) -> 'ok'.

-spec fork(port()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.
-spec fork(port(),[integer()]) -> {'ok', non_neg_integer()} | {'error', file:posix()}.

-spec getcwd(port()) -> {'ok', binary()} | {'error', file:posix()}.
-spec getcwd(port(),list(integer())) -> {'ok', binary()} | {'error', file:posix()}.

-spec getgid(port()) -> non_neg_integer().
-spec getgid(port(),list(integer())) -> non_neg_integer().

-spec gethostname(port()) -> {'ok', binary()} | {'error', file:posix()}.
-spec gethostname(port(),list(integer())) -> {'ok', binary()} | {'error', file:posix()}.

-spec getpid(port()) -> non_neg_integer().
-spec getpid(port(),list(integer())) -> non_neg_integer().

-spec getrlimit(port(),non_neg_integer()) -> {'ok', #rlimit{}} | {'error', file:posix()}.
-spec getrlimit(port(),list(integer()),non_neg_integer()) -> {'ok', #rlimit{}} | {'error', file:posix()}.

-spec getuid(port()) -> non_neg_integer().
-spec getuid(port(),list(integer())) -> non_neg_integer().

-spec kill(port(), integer(), integer()) -> 'ok' | {'error', file:posix()}.
-spec kill(port(), [integer()], integer(), integer()) -> 'ok' | {'error', file:posix()}.

-spec mount(port(),iodata(),iodata(),iodata(),integer(),iodata()) -> 'ok' | {'ok', file:posix()}.
-spec mount(port(),[integer()],iodata(),iodata(),iodata(),integer(),iodata()) -> 'ok' | {'ok', file:posix()}.

-spec mount_define(port(),atom()) -> 'false' | non_neg_integer().
-spec mount_define(port(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec pid(port()) -> [integer()].
-spec pid(port(),[integer()]) -> [integer()].

-type prctl_arg() :: iodata() | non_neg_integer().
-type prctl_val() :: binary() | non_neg_integer().

-spec prctl(port(),integer(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()}.
-spec prctl(port(),[integer()],integer(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()}.

-spec prctl_define(port(),atom()) -> 'false' | non_neg_integer().
-spec prctl_define(port(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec rlimit_define(port(),atom()) -> 'false' | non_neg_integer().
-spec rlimit_define(port(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec setgid(port(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec setgid(port(),list(integer()),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec sethostname(port(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec sethostname(port(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec setns(port(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec setns(port(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec setrlimit(port(),non_neg_integer(),#rlimit{}) -> 'ok' | {'error', file:posix()}.
-spec setrlimit(port(),list(integer()),non_neg_integer(),#rlimit{}) -> 'ok' | {'error', file:posix()}.

-spec setuid(port(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec setuid(port(),list(integer()),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec sigaction(port(),integer(),atom()) -> 'ok' | {'error', file:posix()}.
-spec sigaction(port(),[integer()],integer(),atom()) -> 'ok' | {'error', file:posix()}.

-spec signal_define(port(),atom()) -> 'false' | non_neg_integer().
-spec signal_define(port(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec stderr(port(),list(integer()),'infinity' | non_neg_integer()) -> 'false' | binary().
-spec stdin(port(),list(integer()),iodata()) -> 'true'.
-spec stdout(port(),list(integer()),'infinity' | non_neg_integer()) -> 'false' | binary().

-spec umount(port(),iodata()) -> 'ok' | {error, file:posix()}.
-spec umount(port(),[integer()],iodata()) -> 'ok' | {error, file:posix()}.

-spec unshare(port(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec unshare(port(),[integer()],non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec version(port()) -> binary() | {'error', 'timedout'}.
-spec version(port(),list(integer())) -> binary() | {'error', 'timedout'}.
".
