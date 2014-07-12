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
-type os_pid() :: non_neg_integer().
-type fork_path() :: [os_pid()].

-type fd() :: integer().
-type fd_set() :: [fd()].

-export_type([os_pid/0,fork_path/0]).

-spec audit_arch() -> atom().

-spec chdir(alcove_drv:ref(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chdir(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.

-spec chmod(alcove_drv:ref(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.
-spec chmod(alcove_drv:ref(),fork_path(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.

-spec chown(alcove_drv:ref(),iodata(),non_neg_integer(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec chown(alcove_drv:ref(),fork_path(),iodata(),non_neg_integer(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec chroot(alcove_drv:ref(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chroot(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.

-spec clearenv(alcove_drv:ref()) -> 'ok' | {'error', file:posix()}.
-spec clearenv(alcove_drv:ref(),fork_path()) -> 'ok' | {'error', file:posix()}.

-spec clone(alcove_drv:ref(),non_neg_integer()) -> {'ok', os_pid()} | {'error', file:posix()}.
-spec clone(alcove_drv:ref(),fork_path(),non_neg_integer()) -> {'ok', os_pid()} | {'error', file:posix()}.

-spec clone_define(alcove_drv:ref(),atom()) -> 'false' | non_neg_integer().
-spec clone_define(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().

-spec close(alcove_drv:ref(),fd()) -> 'ok' | {'error', file:posix()}.
-spec close(alcove_drv:ref(),fork_path(),fd()) -> 'ok' | {'error', file:posix()}.

-spec define(alcove_drv:ref(),atom() | [atom()]) -> 'false' | integer().
-spec define(alcove_drv:ref(),fork_path(),atom() | [atom()]) -> 'false' | integer().

-spec eof(alcove_drv:ref(),fork_path()) -> 'ok' | {'error',file:posix()}.
-spec eof(alcove_drv:ref(),fork_path(),'stdin' | 'stdout' | 'stderr') -> 'ok' | {'error',file:posix()}.

-spec environ(alcove_drv:ref()) -> [binary()].
-spec environ(alcove_drv:ref(),fork_path()) -> [binary()].

-spec execve(alcove_drv:ref(),iodata(),[iodata()],[iodata()]) -> 'ok'.
-spec execve(alcove_drv:ref(),fork_path(),iodata(),[iodata()],[iodata()]) -> 'ok'.

-spec execvp(alcove_drv:ref(),iodata(),[iodata()]) -> 'ok'.
-spec execvp(alcove_drv:ref(),fork_path(),iodata(),[iodata()]) -> 'ok'.

-spec exit(alcove_drv:ref(),integer()) -> 'ok'.
-spec exit(alcove_drv:ref(),fork_path(),integer()) -> 'ok'.

-spec file_define(alcove_drv:ref(),atom()) -> non_neg_integer() | 'false'.
-spec file_define(alcove_drv:ref(),fork_path(),atom()) -> non_neg_integer() | 'false'.

-spec fork(alcove_drv:ref()) -> {'ok', os_pid()} | {'error', file:posix()}.
-spec fork(alcove_drv:ref(),fork_path()) -> {'ok', os_pid()} | {'error', file:posix()}.

-spec getcwd(alcove_drv:ref()) -> {'ok', binary()} | {'error', file:posix()}.
-spec getcwd(alcove_drv:ref(),fork_path()) -> {'ok', binary()} | {'error', file:posix()}.

-spec getenv(alcove_drv:ref(),iodata()) -> binary() | 'false'.
-spec getenv(alcove_drv:ref(),fork_path(),iodata()) -> binary() | 'false'.

-spec getgid(alcove_drv:ref()) -> non_neg_integer().
-spec getgid(alcove_drv:ref(),fork_path()) -> non_neg_integer().

-spec gethostname(alcove_drv:ref()) -> {'ok', binary()} | {'error', file:posix()}.
-spec gethostname(alcove_drv:ref(),fork_path()) -> {'ok', binary()} | {'error', file:posix()}.

-spec getopt(alcove_drv:ref(),atom()) -> 'false' | non_neg_integer().
-spec getopt(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().

-spec getpgrp(alcove_drv:ref()) -> integer().
-spec getpgrp(alcove_drv:ref(),fork_path()) -> integer().

-spec getpid(alcove_drv:ref()) -> os_pid().
-spec getpid(alcove_drv:ref(),fork_path()) -> os_pid().

-spec getrlimit(alcove_drv:ref(),non_neg_integer()) -> {'ok', #alcove_rlimit{}} | {'error', file:posix()}.
-spec getrlimit(alcove_drv:ref(),fork_path(),non_neg_integer()) -> {'ok', #alcove_rlimit{}} | {'error', file:posix()}.

-spec getsid(alcove_drv:ref(), integer()) -> {'ok', integer()} | {'error', file:posix()}.
-spec getsid(alcove_drv:ref(), fork_path(), integer()) -> {'ok', integer()} | {'error', file:posix()}.

-spec getuid(alcove_drv:ref()) -> non_neg_integer().
-spec getuid(alcove_drv:ref(),fork_path()) -> non_neg_integer().

-spec kill(alcove_drv:ref(), integer(), integer()) -> 'ok' | {'error', file:posix()}.
-spec kill(alcove_drv:ref(), fork_path(), integer(), integer()) -> 'ok' | {'error', file:posix()}.

-spec lseek(alcove_drv:ref(),integer(),integer(),integer()) -> 'ok' | {'error', file:posix()}.
-spec lseek(alcove_drv:ref(),fork_path(),integer(),integer(),integer()) -> 'ok' | {'error', file:posix()}.

-spec mkdir(alcove_drv:ref(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.
-spec mkdir(alcove_drv:ref(),fork_path(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.

-spec mount(alcove_drv:ref(),iodata(),iodata(),iodata(),integer(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec mount(alcove_drv:ref(),fork_path(),iodata(),iodata(),iodata(),integer(),iodata()) -> 'ok' | {'error', file:posix()}.

-spec mount_define(alcove_drv:ref(),atom()) -> 'false' | non_neg_integer().
-spec mount_define(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().

-spec open(alcove_drv:ref(),iodata(),integer(),integer()) -> {'ok',fd()} | {'error', file:posix()}.
-spec open(alcove_drv:ref(),fork_path(),iodata(),integer(),integer()) -> {'ok',fd()} | {'error', file:posix()}.

-spec pid(alcove_drv:ref()) -> [#alcove_pid{}].
-spec pid(alcove_drv:ref(),fork_path()) -> [#alcove_pid{}].

-type prctl_arg() :: [binary() | {ptr, binary() | non_neg_integer()} ] | binary() | non_neg_integer().
-type prctl_val() :: binary() | non_neg_integer().

-spec prctl(alcove_drv:ref(),integer(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()}.
-spec prctl(alcove_drv:ref(),fork_path(),integer(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()}.

-spec prctl_define(alcove_drv:ref(),atom()) -> 'false' | non_neg_integer().
-spec prctl_define(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().

-spec read(alcove_drv:ref(),fd(),non_neg_integer()) -> {'ok', binary()} | {'error', file:posix()}.
-spec read(alcove_drv:ref(),fork_path(),fd(),non_neg_integer()) -> {'ok', binary()} | {'error', file:posix()}.

-spec readdir(alcove_drv:ref(),iodata()) -> {'ok', [binary()]} | {'error', file:posix()}.
-spec readdir(alcove_drv:ref(),fork_path(),iodata()) -> {'ok', [binary()]} | {'error', file:posix()}.

-spec rmdir(alcove_drv:ref(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec rmdir(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.

-spec rlimit_define(alcove_drv:ref(),atom()) -> 'false' | non_neg_integer().
-spec rlimit_define(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().

-spec select(alcove_drv:ref(),[fd_set()],[fd_set()],[fd_set()],
    <<>> | #alcove_timeval{}) -> {ok, [fd_set()], [fd_set()], [fd_set()]} | {'error', file:posix()}.
-spec select(alcove_drv:ref(),fork_path(),[fd_set()],[fd_set()],[fd_set()],
    <<>> | #alcove_timeval{}) -> {ok, [fd_set()], [fd_set()], [fd_set()]} | {'error', file:posix()}.

-spec setenv(alcove_drv:ref(),iodata(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.
-spec setenv(alcove_drv:ref(),fork_path(),iodata(),iodata(),integer()) -> 'ok' | {'error', file:posix()}.

-spec setgid(alcove_drv:ref(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec setgid(alcove_drv:ref(),fork_path(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec sethostname(alcove_drv:ref(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec sethostname(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.

-spec setns(alcove_drv:ref(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec setns(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.

-spec setopt(alcove_drv:ref(),atom(), non_neg_integer()) -> boolean().
-spec setopt(alcove_drv:ref(),fork_path(),atom(),non_neg_integer()) -> boolean().

-spec setrlimit(alcove_drv:ref(),non_neg_integer(),#alcove_rlimit{}) -> 'ok' | {'error', file:posix()}.
-spec setrlimit(alcove_drv:ref(),fork_path(),non_neg_integer(),#alcove_rlimit{}) -> 'ok' | {'error', file:posix()}.

-spec setuid(alcove_drv:ref(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec setuid(alcove_drv:ref(),fork_path(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec sigaction(alcove_drv:ref(),integer(),atom()) -> 'ok' | {'error', file:posix()}.
-spec sigaction(alcove_drv:ref(),fork_path(),integer(),atom()) -> 'ok' | {'error', file:posix()}.

-spec signal_constant(alcove_drv:ref(),non_neg_integer()) -> 'false' | atom().
-spec signal_constant(alcove_drv:ref(),fork_path(),non_neg_integer()) -> 'false' | atom().

-spec signal_define(alcove_drv:ref(),atom()) -> 'false' | non_neg_integer().
-spec signal_define(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().

-spec syscall_define(alcove_drv:ref(),atom()) -> 'false' | non_neg_integer().
-spec syscall_define(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().

-spec stderr(alcove_drv:ref()) -> 'false' | binary().
-spec stderr(alcove_drv:ref(),fork_path()) -> 'false' | binary().
-spec stderr(alcove_drv:ref(),fork_path(),'infinity' | non_neg_integer()) -> 'false' | binary().

-spec stdin(alcove_drv:ref(),iodata()) -> 'true'.
-spec stdin(alcove_drv:ref(),fork_path(),iodata()) -> 'true'.

-spec stdout(alcove_drv:ref()) -> 'false' | binary().
-spec stdout(alcove_drv:ref(),fork_path()) -> 'false' | binary().
-spec stdout(alcove_drv:ref(),fork_path(),'infinity' | non_neg_integer()) -> 'false' | binary().

-spec umount(alcove_drv:ref(),iodata()) -> 'ok' | {error, file:posix()}.
-spec umount(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {error, file:posix()}.

-spec unsetenv(alcove_drv:ref(),iodata()) -> 'ok' | {error, file:posix()}.
-spec unsetenv(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {error, file:posix()}.

-spec unshare(alcove_drv:ref(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.
-spec unshare(alcove_drv:ref(),fork_path(),non_neg_integer()) -> 'ok' | {'error', file:posix()}.

-spec write(alcove_drv:ref(),fd(),iodata()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.
-spec write(alcove_drv:ref(),fork_path(),fd(),iodata()) -> {'ok', non_neg_integer()} | {'error', file:posix()}.

-spec version(alcove_drv:ref()) -> binary().
-spec version(alcove_drv:ref(),fork_path()) -> binary().
".
