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
                    % name(Drv, ...) -> alcove:call(Drv, [], Fun, [...])
                    Arg = arg("Arg", Arity),

                    Pattern0 = [erl_syntax:variable("Drv")|Arg],
                    Body0 = erl_syntax:application(
                        erl_syntax:atom(call),
                        [erl_syntax:variable("Drv"), erl_syntax:nil(),
                            erl_syntax:atom(Fun), erl_syntax:list(Arg)]
                    ),
                    Clause0 = erl_syntax:clause(Pattern0, [], [Body0]),

                    % name(Drv, Pids, ...) -> alcove:call(Drv, Pids, Fun, [...])
                    Pattern1 = [erl_syntax:variable("Drv"), erl_syntax:variable("Pids")|Arg],
                    Body1 = erl_syntax:application(
                        erl_syntax:atom(call),
                        [erl_syntax:variable("Drv"), erl_syntax:variable("Pids"),
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
    [{audit_arch,0},

     {define,2},{define,3},
     {stdin,2}, {stdin,3},
     {stdout,1}, {stdout,2}, {stdout,3},
     {stderr,1}, {stderr,2}, {stderr,3},
     {eof,2}, {eof,3},
     {event,1}, {event,2}, {event,3},
     {encode,3},
     {command,1},
     {cast,2}, {cast,3}, {cast,4},
     {call,2},
     {call,3},
     {call,4}].

static() ->
    [ static({Fun, Arity}) || {Fun, Arity} <- static_exports() ].

static({audit_arch,0}) ->
"audit_arch() ->
    Arches = [
        {{\"armv6l\",\"linux\",4}, 'AUDIT_ARCH_ARM'},
        {{\"armv7l\",\"linux\",4}, 'AUDIT_ARCH_ARM'},
        {{\"i386\",\"linux\",4}, 'AUDIT_ARCH_I386'},
        {{\"x86_64\",\"linux\",8}, 'AUDIT_ARCH_X86_64'}
    ],
    [Arch,_,OS|_] = string:tokens(
        erlang:system_info(system_architecture),
        \"-\"
    ),
    Wordsize = erlang:system_info({wordsize,external}),
    proplists:get_value({Arch,OS,Wordsize}, Arches, unsupported).
";

static({define,2}) ->
"
define(Drv, Const) ->
    define(Drv, [], Const).
";
static({define,3}) ->
"
define(Drv, Pids, Const) when is_atom(Const) ->
    define(Drv, Pids, [Const]);
define(Drv, Pids, Consts) when is_list(Consts) ->
    lists:foldl(fun(Const,A) ->
                N = case atom_to_list(Const) of
                    \"CLONE_\" ++ _ ->
                        alcove:clone_define(Drv, Pids, Const);
                    \"MS_\" ++ _ ->
                        alcove:mount_define(Drv, Pids, Const);
                    \"MNT_\" ++ _ ->
                        alcove:mount_define(Drv, Pids, Const);
                    \"O_\" ++ _ ->
                        alcove:file_define(Drv, Pids, Const);
                    \"PR_\" ++ _ ->
                        alcove:prctl_define(Drv, Pids, Const);
                    \"SECCOMP_\" ++ _ ->
                        alcove:prctl_define(Drv, Pids, Const);
                    \"RLIMIT_\" ++ _ ->
                        alcove:rlimit_define(Drv, Pids, Const);
                    \"AUDIT_ARCH_\" ++ _ ->
                        alcove:syscall_define(Drv, Pids, Const);
                    \"__NR_\" ++ _ ->
                        alcove:syscall_define(Drv, Pids, Const);
                    \"SYS_\" ++ Rest ->
                        alcove:syscall_define(Drv, Pids,
                            list_to_atom(\"__NR_\" ++ Rest));
                    \"SIG\" ++ _ ->
                        alcove:signal_define(Drv, Pids, Const);
                    Flag when
                        Flag =:= \"rdonly\";
                        Flag =:= \"nosuid\";
                        Flag =:= \"noexec\";
                        Flag =:= \"noatime\" ->
                            alcove:mount_define(Drv, Pids, Const);
                    _ -> false
                end,
                if
                    is_integer(N) -> N bxor A;
                    true -> false
                end
        end,
        0,
        Consts).
";

static({stdin,2}) ->
"
stdin(Drv, Data) ->
    stdin(Drv, [], Data).
";
static({stdin,3}) ->
"
stdin(Drv, Pids, Data) ->
    alcove_drv:stdin(Drv, Pids, Data).
";

static({stdout,1}) ->
"
stdout(Drv) ->
    stdout(Drv, [], 0).
";
static({stdout,2}) ->
"
stdout(Drv, Pids) ->
    stdout(Drv, Pids, 0).
";
static({stdout,3}) ->
"
stdout(Drv, Pids, Timeout) ->
    alcove_drv:stdout(Drv, Pids, Timeout).
";

static({stderr,1}) ->
"
stderr(Drv) ->
    stderr(Drv, [], 0).
";
static({stderr,2}) ->
"
stderr(Drv, Pids) ->
    stderr(Drv, Pids, 0).
";
static({stderr,3}) ->
"
stderr(Drv, Pids, Timeout) ->
    alcove_drv:stderr(Drv, Pids, Timeout).
";

static({eof,2}) ->
"
eof(Drv, Pids) ->
    eof(Drv, Pids, stdin).
";
static({eof,3}) ->
"
eof(_Drv, [], _Stdio) ->
    {error,esrch};
eof(Drv, Pids0, Stdio) ->
    [Pid|Rest] = lists:reverse(Pids0),
    Pids = lists:reverse(Rest),
    Proc = pid(Drv, Pids),
    case lists:keyfind(Pid, 2, Proc) of
        false ->
            {error,esrch};
        N ->
            eof_1(Drv, Pids, N, Stdio)
    end.

eof_1(Drv, Pids, #alcove_pid{stdin = FD}, stdin) ->
    close(Drv, Pids, FD);
eof_1(Drv, Pids, #alcove_pid{stdout = FD}, stdout) ->
    close(Drv, Pids, FD);
eof_1(Drv, Pids, #alcove_pid{stderr = FD}, stderr) ->
    close(Drv, Pids, FD).
";

static({event,1}) ->
"
event(Drv) ->
    event(Drv, [], 0).
";
static({event,2}) ->
"
event(Drv, Pids) ->
    event(Drv, Pids, 0).
";
static({event,3}) ->
"
event(Drv, Pids, Timeout) ->
    alcove_drv:event(Drv, Pids, Timeout).
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

static({cast,2}) ->
"
cast(Drv, Command) ->
    cast(Drv, [], Command, []).
";
static({cast,3}) ->
"
cast(Drv, Command, Options) ->
    cast(Drv, [], Command, Options).
";
static({cast,4}) ->
"
cast(Drv, Pids, Command, Arg) when is_pid(Drv), is_list(Arg) ->
    alcove_drv:send(Drv, encode(Command, Pids, Arg)).
";

static({call,2}) ->
"
call(Drv, Command) ->
    call(Drv, [], Command, []).
";
static({call,3}) ->
"
call(Drv, Command, Options) ->
    call(Drv, [], Command, Options).
";
static({call,4}) ->
"
call(Drv, Pids, Command, Arg) when is_pid(Drv), is_list(Arg) ->
    case alcove_drv:call(Drv, Pids, encode(Command, Pids, Arg)) of
        badarg ->
            erlang:error(badarg, [Drv, Command, Arg]);
        Reply ->
            Reply
    end.
".

includes(Header) ->
    [ erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header ].

% FIXME hack for hard coding typespecs
specs() ->
"
-spec chdir(pid(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chdir(pid(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec chmod(pid(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.
-spec chmod(pid(),[integer()],iodata(),integer()) -> 'ok' | {'error', file:posix()}.

-spec chown(pid(),iodata(),non_neg_integer(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec chown(pid(),[integer()],iodata(),non_neg_integer(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec chroot(pid(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chroot(pid(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec clearenv(pid()) -> 'ok' | {'error', file:posix()}.
-spec clearenv(pid(),[integer()]) -> 'ok' | {'error', file:posix()}.

-spec clone(pid(),non_neg_integer()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.
-spec clone(pid(),[integer()],non_neg_integer()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.

-spec clone_define(pid(),atom()) -> 'false' | non_neg_integer().
-spec clone_define(pid(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec close(pid(),integer()) -> 'ok' | {'error', file:posix()}.
-spec close(pid(),[integer()],integer()) -> 'ok' | {'error', file:posix()}.

-spec define(pid(),atom() | [atom()]) -> 'false' | integer().

-spec environ(pid()) -> [binary()].
-spec environ(pid(),[integer()]) -> [binary()].

-spec execve(pid(),iodata(),[iodata()],[iodata()]) -> 'ok'.
-spec execve(pid(),list(integer()),iodata(),[iodata()],[iodata()]) -> 'ok'.

-spec execvp(pid(),iodata(),[iodata()]) -> 'ok'.
-spec execvp(pid(),list(integer()),iodata(),[iodata()]) -> 'ok'.

-spec exit(pid(),integer()) -> 'ok'.
-spec exit(pid(),list(integer()),integer()) -> 'ok'.

-spec fork(pid()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.
-spec fork(pid(),[integer()]) -> {'ok', non_neg_integer()} | {'error', file:posix()}.

-spec getcwd(pid()) -> {'ok', binary()} | {'error', file:posix()}.
-spec getcwd(pid(),list(integer())) -> {'ok', binary()} | {'error', file:posix()}.

-spec getenv(pid(),iodata()) -> binary() | 'false'.
-spec getenv(pid(),list(integer()),iodata()) -> binary() | 'false'.

-spec getgid(pid()) -> non_neg_integer().
-spec getgid(pid(),list(integer())) -> non_neg_integer().

-spec gethostname(pid()) -> {'ok', binary()} | {'error', file:posix()}.
-spec gethostname(pid(),list(integer())) -> {'ok', binary()} | {'error', file:posix()}.

-spec getopt(pid(),atom()) -> 'false' | non_neg_integer().
-spec getopt(pid(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec getpgrp(pid()) -> integer().
-spec getpgrp(pid(),list(integer())) -> integer().

-spec getpid(pid()) -> non_neg_integer().
-spec getpid(pid(),list(integer())) -> non_neg_integer().

-spec getrlimit(pid(),non_neg_integer()) -> {'ok', #alcove_rlimit{}} | {'error', file:posix()}.
-spec getrlimit(pid(),list(integer()),non_neg_integer()) -> {'ok', #alcove_rlimit{}} | {'error', file:posix()}.

-spec getsid(pid(), integer()) -> {'ok', integer()} | {'error', file:posix()}.
-spec getsid(pid(), [integer()], integer()) -> {'ok', integer()} | {'error', file:posix()}.

-spec getuid(pid()) -> non_neg_integer().
-spec getuid(pid(),list(integer())) -> non_neg_integer().

-spec kill(pid(), integer(), integer()) -> 'ok' | {'error', file:posix()}.
-spec kill(pid(), [integer()], integer(), integer()) -> 'ok' | {'error', file:posix()}.

-spec lseek(pid(),integer(),integer(),integer()) -> 'ok' | {'error', file:posix()}.
-spec lseek(pid(),[integer()],integer(),integer(),integer()) -> 'ok' | {'error', file:posix()}.

-spec mkdir(pid(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.
-spec mkdir(pid(),[integer()],iodata(),integer()) -> 'ok' | {'error', file:posix()}.

-spec mount(pid(),iodata(),iodata(),iodata(),integer(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec mount(pid(),[integer()],iodata(),iodata(),iodata(),integer(),iodata()) -> 'ok' | {'error', file:posix()}.

-spec mount_define(pid(),atom()) -> 'false' | non_neg_integer().
-spec mount_define(pid(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec open(pid(),iodata(),integer(),integer()) -> {'ok',integer()} | {'error', file:posix()}.
-spec open(pid(),[integer()],iodata(),integer(),integer()) -> {'ok',integer()} | {'error', file:posix()}.

-spec pid(pid()) -> [#alcove_pid{}].
-spec pid(pid(),[integer()]) -> [#alcove_pid{}].

-type prctl_arg() :: [binary() | {ptr, binary() | non_neg_integer()} ] | binary() | non_neg_integer().
-type prctl_val() :: binary() | non_neg_integer().

-spec prctl(pid(),integer(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()}.
-spec prctl(pid(),[integer()],integer(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()}.

-spec prctl_define(pid(),atom()) -> 'false' | non_neg_integer().
-spec prctl_define(pid(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec read(pid(),integer(),non_neg_integer()) -> {'ok', binary()} | {'error', file:posix()}.
-spec read(pid(),[integer()],integer(),non_neg_integer()) -> {'ok', binary()} | {'error', file:posix()}.

-spec readdir(pid(),iodata()) -> {'ok', [binary()]} | {'error', file:posix()}.
-spec readdir(pid(),[integer()],iodata()) -> {'ok', [binary()]} | {'error', file:posix()}.

-spec rlimit_define(pid(),atom()) -> 'false' | non_neg_integer().
-spec rlimit_define(pid(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec select(pid(),[integer()],[integer()],[integer()],
    <<>> | #alcove_timeval{}) -> {ok, [integer()], [integer()], [integer()]} | {'error', file:posix()}.
-spec select(pid(),[integer()],[integer()],[integer()],[integer()],
    <<>> | #alcove_timeval{}) -> {ok, [integer()], [integer()], [integer()]} | {'error', file:posix()}.

-spec setenv(pid(),iodata(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.
-spec setenv(pid(),list(integer()),iodata(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.

-spec setgid(pid(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec setgid(pid(),list(integer()),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec sethostname(pid(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec sethostname(pid(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec setns(pid(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec setns(pid(),list(integer()),iodata()) -> 'ok' | {'error', file:posix()}.

-spec setopt(pid(),atom(), non_neg_integer()) -> boolean().
-spec setopt(pid(),[integer()],atom(),non_neg_integer()) -> boolean().

-spec setrlimit(pid(),non_neg_integer(),#alcove_rlimit{}) -> 'ok' | {'error', file:posix()}.
-spec setrlimit(pid(),list(integer()),non_neg_integer(),#alcove_rlimit{}) -> 'ok' | {'error', file:posix()}.

-spec setuid(pid(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec setuid(pid(),list(integer()),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec sigaction(pid(),integer(),atom()) -> 'ok' | {'error', file:posix()}.
-spec sigaction(pid(),[integer()],integer(),atom()) -> 'ok' | {'error', file:posix()}.

-spec signal_constant(pid(),non_neg_integer()) -> 'false' | atom().
-spec signal_constant(pid(),[integer()],non_neg_integer()) -> 'false' | atom().

-spec signal_define(pid(),atom()) -> 'false' | non_neg_integer().
-spec signal_define(pid(),[integer()],atom()) -> 'false' | non_neg_integer().

-spec stderr(pid(),list(integer()),'infinity' | non_neg_integer()) -> 'false' | binary().
-spec stdin(pid(),list(integer()),iodata()) -> 'true'.
-spec stdout(pid(),list(integer()),'infinity' | non_neg_integer()) -> 'false' | binary().

-spec umount(pid(),iodata()) -> 'ok' | {error, file:posix()}.
-spec umount(pid(),[integer()],iodata()) -> 'ok' | {error, file:posix()}.

-spec unsetenv(pid(),iodata()) -> 'ok' | {error, file:posix()}.
-spec unsetenv(pid(),[integer()],iodata()) -> 'ok' | {error, file:posix()}.

-spec unshare(pid(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec unshare(pid(),[integer()],non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec write(pid(),integer(),iodata()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.
-spec write(pid(),[integer()],integer(),iodata()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.

-spec version(pid()) -> binary() | {'error', 'timedout'}.
-spec version(pid(),list(integer())) -> binary() | {'error', 'timedout'}.
".
