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

    Exports_gen1 = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity+2))
                        || {Fun, Arity} <- Calls ])
                ]),

    Exports_gen2 = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity+3))
                        || {Fun, Arity} <- Calls ])
                ]),

    % Generate the functions
    Functions = [ begin
                    % name(Drv, ...) -> alcove:call(Drv, [], Fun, [...])
                    Arg = arg("Arg", Arity),

                    % name(Drv, Pids, ...) -> name(Drv, Pids, ..., infinity)
                    Pattern1 = [erl_syntax:variable("Drv"), erl_syntax:variable("Pids")|Arg],
                    Body1 = erl_syntax:application(
                        erl_syntax:atom(Fun),
                        lists:flatten([erl_syntax:variable("Drv"), erl_syntax:variable("Pids"), Arg, erl_syntax:atom(infinity)])
                    ),
                    Clause1 = erl_syntax:clause(Pattern1, [], [Body1]),

                    % name(Drv, Pids, ..., Timeout) -> alcove:call(Drv, Pids, Fun, [...], Timeout)
                    Pattern2 = lists:flatten([erl_syntax:variable("Drv"), erl_syntax:variable("Pids"), Arg, erl_syntax:variable("Timeout")]),
                    Body2 = erl_syntax:application(
                        erl_syntax:atom(call),
                        [erl_syntax:variable("Drv"), erl_syntax:variable("Pids"),
                            erl_syntax:atom(Fun), erl_syntax:list(Arg),
                            erl_syntax:variable("Timeout")]
                    ),
                    Clause2 = erl_syntax:clause(Pattern2, [], [Body2]),

                    [erl_syntax:function(erl_syntax:atom(Fun), [Clause1]),
                        erl_syntax:function(erl_syntax:atom(Fun), [Clause2])]

                end || {Fun, Arity} <- Calls ],

    Code0 = erl_prettypr:format(erl_syntax:form_list(lists:flatten([
                license(),
                Module,
                Includes,

                Specs,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen1,
                Exports_gen2,

                Static,
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

     {define,3},
     {stdin,3},
     {stdout,2}, {stdout,3},
     {stderr,2}, {stderr,3},
     {eof,2}, {eof,3},
     {event,2}, {event,3},
     {call,4}, {call,5}].

static() ->
    [ static({Fun, Arity}) || {Fun, Arity} <- static_exports() ].

static({audit_arch,0}) ->
"audit_arch() ->
    Arches = [
        {{\"armv6l\",\"linux\",4}, audit_arch_arm},
        {{\"armv7l\",\"linux\",4}, audit_arch_arm},
        {{\"i386\",\"linux\",4}, audit_arch_i386},
        {{\"x86_64\",\"linux\",8}, audit_arch_x86_64}
    ],
    [Arch,_,OS|_] = string:tokens(
        erlang:system_info(system_architecture),
        \"-\"
    ),
    Wordsize = erlang:system_info({wordsize,external}),
    proplists:get_value({Arch,OS,Wordsize}, Arches, unsupported).
";

static({define,3}) ->
"
define(Drv, Pids, Const) when is_atom(Const) ->
    define(Drv, Pids, [Const]);
define(Drv, Pids, Consts0) when is_list(Consts0) ->
    Consts = [ begin
                case atom_to_list(Const) of
                    \"sys_\" ++ Rest -> \"__nr_\" ++ Rest;
                    X -> X
                end
        end || Const <- Consts0 ],

    try lists:foldl(fun(Const, Acc) ->
                Fun = const(Const),
                Acc bxor alcove:Fun(Drv, Pids, list_to_atom(Const))
        end, 0, Consts)
    catch
        error:badarith -> unknown;
        error:function_clause -> unknown
    end.

const(\"clone_\" ++ _) -> clone_define;
const(\"ms_\" ++ _) -> mount_define;
const(\"mnt_\" ++ _) -> mount_define;
const(\"o_\" ++ _) -> file_define;
const(\"pr_\" ++ _) -> prctl_define;
const(\"seccomp_\" ++ _) -> prctl_define;
const(\"rlimit_\" ++ _) -> rlimit_define;
const(\"audit_arch_\" ++ _) -> syscall_define;
const(\"__nr_\" ++ _) -> syscall_define;
const(\"sig\" ++ _) -> signal_define;
const(\"rdonly\") -> mount_define;
const(\"nosuid\") -> mount_define;
const(\"noexec\") -> mount_define;
const(\"noatime\") -> mount_define.
";

static({stdin,3}) ->
"
stdin(Drv, Pids, Data) ->
    alcove_drv:stdin(Drv, Pids, Data).
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

static({call,4}) ->
"
call(Drv, Pids, Command, Argv) ->
    call(Drv, Pids, Command, Argv, infinity).
";
static({call,5}) ->
"
call(Drv, Pids, Command, Argv, Timeout) when is_pid(Drv), is_list(Argv) ->
    case alcove_drv:call(Drv, Pids, Command, Argv, Timeout) of
        Error when Error =:= badarg; Error =:= undef ->
            erlang:error(Error, [Drv, Pids, Command, Argv, Timeout]);
        Reply ->
            Reply
    end.
".

includes(Header) ->
    [ erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header ].

% FIXME hack for hard coding typespecs
specs() ->
"
-type uint8_t() :: 0 .. 16#ff.
-type uint16_t() :: 0 .. 16#ffff.
-type uint32_t() :: 0 .. 16#ffffffff.
-type uint64_t() :: 0 .. 16#ffffffffffffffff.

-type int8_t() :: -16#7f .. 16#7f.
-type int16_t() :: -16#7fff .. 16#7fff.
-type int32_t() :: -16#7fffffff .. 16#7fffffff.
-type int64_t() :: -16#7fffffffffffffff .. 16#7fffffffffffffff.

-type mode_t() :: uint32_t().
-type uid_t() :: uint32_t().
-type gid_t() :: uint32_t().
-type off_t() :: uint64_t().
-type size_t() :: uint64_t().
-type ssize_t() :: int64_t().

-type pid_t() :: int32_t().
-type fork_path() :: [pid_t()].

-type fd() :: int32_t().
-type fd_set() :: [fd()].

-type constant() :: atom() | integer().

-export_type([
        uint8_t/0, uint16_t/0, uint32_t/0, uint64_t/0,
        int8_t/0, int16_t/0, int32_t/0, int64_t/0,

        mode_t/0,
        uid_t/0,
        gid_t/0,
        off_t/0,
        size_t/0,
        ssize_t/0,
        fd/0,
        fd_set/0,

        pid_t/0,
        fork_path/0,
        constant/0
    ]).

-spec audit_arch() -> atom().

-spec call(alcove_drv:ref(),fork_path(),atom(),list()) -> term().
-spec call(alcove_drv:ref(),fork_path(),atom(),list(),timeout()) -> term().

-spec chdir(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chdir(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec chmod(alcove_drv:ref(),fork_path(),iodata(),mode_t()) -> 'ok' | {'error', file:posix()}.
-spec chmod(alcove_drv:ref(),fork_path(),iodata(),mode_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec chown(alcove_drv:ref(),fork_path(),iodata(),uid_t(),gid_t()) -> 'ok' | {'error', file:posix()}.
-spec chown(alcove_drv:ref(),fork_path(),iodata(),uid_t(),gid_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec chroot(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec chroot(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec clearenv(alcove_drv:ref(),fork_path()) -> 'ok' | {'error', file:posix()}.
-spec clearenv(alcove_drv:ref(),fork_path(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec clone(alcove_drv:ref(),fork_path(),int32_t() | [constant()]) -> {'ok', pid_t()} | {'error', file:posix() | 'unsupported'}.
-spec clone(alcove_drv:ref(),fork_path(),int32_t() | [constant()],timeout()) -> {'ok', pid_t()} | {'error', file:posix() | 'unsupported'}.

-spec clone_define(alcove_drv:ref(),fork_path(),atom()) -> 'unknown' | int32_t().
-spec clone_define(alcove_drv:ref(),fork_path(),atom(),timeout()) -> 'unknown' | int32_t().

-spec close(alcove_drv:ref(),fork_path(),fd()) -> 'ok' | {'error', file:posix()}.
-spec close(alcove_drv:ref(),fork_path(),fd(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec define(alcove_drv:ref(),fork_path(),atom() | [atom()]) -> 'unknown' | integer().

-spec environ(alcove_drv:ref(),fork_path()) -> [binary()].
-spec environ(alcove_drv:ref(),fork_path(),timeout()) -> [binary()].

-spec eof(alcove_drv:ref(),fork_path(),'stdin' | 'stdout' | 'stderr') -> 'ok' | {'error',file:posix()}.

-spec errno_id(alcove_drv:ref(),fork_path(),int32_t()) -> file:posix().
-spec errno_id(alcove_drv:ref(),fork_path(),int32_t(),timeout()) -> file:posix().

-spec event(alcove_drv:ref(),fork_path()) -> term().
-spec event(alcove_drv:ref(),fork_path(),timeout()) -> term().

-spec execve(alcove_drv:ref(),fork_path(),iodata(),[iodata()],[iodata()]) -> 'ok'.
-spec execve(alcove_drv:ref(),fork_path(),iodata(),[iodata()],[iodata()],timeout()) -> 'ok'.

-spec execvp(alcove_drv:ref(),fork_path(),iodata(),[iodata()]) -> 'ok'.
-spec execvp(alcove_drv:ref(),fork_path(),iodata(),[iodata()],timeout()) -> 'ok'.

-spec exit(alcove_drv:ref(),fork_path(),int32_t()) -> 'ok'.
-spec exit(alcove_drv:ref(),fork_path(),int32_t(),timeout()) -> 'ok'.

-spec file_define(alcove_drv:ref(),fork_path(),atom()) -> non_neg_integer() | 'unknown'.
-spec file_define(alcove_drv:ref(),fork_path(),atom(),timeout()) -> non_neg_integer() | 'unknown'.

-spec fork(alcove_drv:ref(),fork_path()) -> {'ok', pid_t()} | {'error', file:posix()}.
-spec fork(alcove_drv:ref(),fork_path(),timeout()) -> {'ok', pid_t()} | {'error', file:posix()}.

-spec getcwd(alcove_drv:ref(),fork_path()) -> {'ok', binary()} | {'error', file:posix()}.
-spec getcwd(alcove_drv:ref(),fork_path(),timeout()) -> {'ok', binary()} | {'error', file:posix()}.

-spec getenv(alcove_drv:ref(),fork_path(),iodata()) -> binary() | 'false'.
-spec getenv(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> binary() | 'false'.

-spec getgid(alcove_drv:ref(),fork_path()) -> gid_t().
-spec getgid(alcove_drv:ref(),fork_path(),timeout()) -> gid_t().

-spec gethostname(alcove_drv:ref(),fork_path()) -> {'ok', binary()} | {'error', file:posix()}.
-spec gethostname(alcove_drv:ref(),fork_path(),timeout()) -> {'ok', binary()} | {'error', file:posix()}.

-spec getopt(alcove_drv:ref(),fork_path(),atom()) -> 'false' | non_neg_integer().
-spec getopt(alcove_drv:ref(),fork_path(),atom(),timeout()) -> 'false' | non_neg_integer().

-spec getpgrp(alcove_drv:ref(),fork_path()) -> pid_t().
-spec getpgrp(alcove_drv:ref(),fork_path(),timeout()) -> pid_t().

-spec getpid(alcove_drv:ref(),fork_path()) -> pid_t().
-spec getpid(alcove_drv:ref(),fork_path(),timeout()) -> pid_t().

-spec getpriority(alcove_drv:ref(),fork_path(),constant(),int32_t()) -> {'ok',int32_t()} | {'error', file:posix() | 'unsupported'}.
-spec getpriority(alcove_drv:ref(),fork_path(),constant(),int32_t(),timeout()) -> {'ok',int32_t()} | {'error', file:posix() | 'unsupported'}.

-spec getresgid(alcove_drv:ref(),fork_path()) -> {'ok', gid_t(), gid_t(), gid_t()} | {'error', file:posix()}.
-spec getresgid(alcove_drv:ref(),fork_path(),timeout()) -> {'ok', gid_t(), gid_t(), gid_t()} | {'error', file:posix()}.

-spec getresuid(alcove_drv:ref(),fork_path()) -> {'ok', uid_t(), uid_t(), uid_t()} | {'error', file:posix()}.
-spec getresuid(alcove_drv:ref(),fork_path(),timeout()) -> {'ok', uid_t(), uid_t(), uid_t()} | {'error', file:posix()}.

-spec getrlimit(alcove_drv:ref(),fork_path(),constant()) -> {'ok', #alcove_rlimit{}} | {'error', file:posix() | 'unsupported'}.
-spec getrlimit(alcove_drv:ref(),fork_path(),constant(),timeout()) -> {'ok', #alcove_rlimit{}} | {'error', file:posix() | 'unsupported'}.

-spec getsid(alcove_drv:ref(),fork_path(),pid_t()) -> {'ok', pid_t()} | {'error', file:posix()}.
-spec getsid(alcove_drv:ref(),fork_path(),pid_t(),timeout()) -> {'ok', pid_t()} | {'error', file:posix()}.

-spec getuid(alcove_drv:ref(),fork_path()) -> uid_t().
-spec getuid(alcove_drv:ref(),fork_path(),timeout()) -> uid_t().

-spec kill(alcove_drv:ref(),fork_path(),pid_t(),constant()) -> 'ok' | {'error', file:posix() | 'unsupported'}.
-spec kill(alcove_drv:ref(),fork_path(),pid_t(),constant(),timeout()) -> 'ok' | {'error', file:posix() | 'unsupported'}.

-spec lseek(alcove_drv:ref(),fork_path(),fd(),off_t(),int32_t()) -> 'ok' | {'error', file:posix()}.
-spec lseek(alcove_drv:ref(),fork_path(),fd(),off_t(),int32_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec mkdir(alcove_drv:ref(),fork_path(),iodata(),mode_t()) -> 'ok' | {'error', file:posix()}.
-spec mkdir(alcove_drv:ref(),fork_path(),iodata(),mode_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec mount(alcove_drv:ref(),fork_path(),iodata(),iodata(),iodata(),uint64_t() | [constant()],iodata(),iodata()) -> 'ok' | {'error', file:posix() | 'unsupported'}.
-spec mount(alcove_drv:ref(),fork_path(),iodata(),iodata(),iodata(),uint64_t() | [constant()],iodata(),iodata(),timeout()) -> 'ok' | {'error', file:posix() | 'unsupported'}.

-spec mount_define(alcove_drv:ref(),fork_path(),atom()) -> 'unknown' | uint64_t().
-spec mount_define(alcove_drv:ref(),fork_path(),atom(),timeout()) -> 'unknown' | uint64_t().

-spec open(alcove_drv:ref(),fork_path(),iodata(),int32_t() | [constant()],mode_t()) -> {'ok',fd()} | {'error', file:posix() | 'unsupported'}.
-spec open(alcove_drv:ref(),fork_path(),iodata(),int32_t() | [constant()],mode_t(),timeout()) -> {'ok',fd()} | {'error', file:posix() | 'unsupported'}.

-spec pid(alcove_drv:ref(),fork_path()) -> [#alcove_pid{}].
-spec pid(alcove_drv:ref(),fork_path(),timeout()) -> [#alcove_pid{}].

-spec pivot_root(alcove_drv:ref(),fork_path(),iodata(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec pivot_root(alcove_drv:ref(),fork_path(),iodata(),iodata(),timeout()) -> 'ok' | {'error', file:posix()}.

-type prctl_arg() :: [binary() | {ptr, binary() | non_neg_integer()} ] | binary() | integer() | atom().
-type prctl_val() :: binary() | integer().

-spec prctl(alcove_drv:ref(),fork_path(),constant(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()} | {'error', file:posix() | 'unsupported'}.
-spec prctl(alcove_drv:ref(),fork_path(),constant(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg(),timeout()) -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()} | {'error', file:posix() | 'unsupported'}.

-spec prctl_define(alcove_drv:ref(),fork_path(),atom()) -> 'unknown' | non_neg_integer().
-spec prctl_define(alcove_drv:ref(),fork_path(),atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec read(alcove_drv:ref(),fork_path(),fd(),size_t()) -> {'ok', binary()} | {'error', file:posix()}.
-spec read(alcove_drv:ref(),fork_path(),fd(),size_t(),timeout()) -> {'ok', binary()} | {'error', file:posix()}.

-spec readdir(alcove_drv:ref(),fork_path(),iodata()) -> {'ok', [binary()]} | {'error', file:posix()}.
-spec readdir(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> {'ok', [binary()]} | {'error', file:posix()}.

-spec rmdir(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec rmdir(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec rlimit_define(alcove_drv:ref(),fork_path(),atom()) -> 'unknown' | non_neg_integer().
-spec rlimit_define(alcove_drv:ref(),fork_path(),atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec select(alcove_drv:ref(),fork_path(),[fd_set()],[fd_set()],[fd_set()],
    <<>> | #alcove_timeval{}) -> {ok, [fd_set()], [fd_set()], [fd_set()]} | {'error', file:posix()}.
-spec select(alcove_drv:ref(),fork_path(),[fd_set()],[fd_set()],[fd_set()],
    <<>> | #alcove_timeval{},timeout()) -> {ok, [fd_set()], [fd_set()], [fd_set()]} | {'error', file:posix()}.

-spec setenv(alcove_drv:ref(),fork_path(),iodata(),iodata(),int32_t()) -> 'ok' | {'error', file:posix()}.
-spec setenv(alcove_drv:ref(),fork_path(),iodata(),iodata(),int32_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec setgid(alcove_drv:ref(),fork_path(),gid_t()) -> 'ok' | {'error', file:posix()}.
-spec setgid(alcove_drv:ref(),fork_path(),gid_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec sethostname(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {'error', file:posix()}.
-spec sethostname(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec setns(alcove_drv:ref(),fork_path(),iodata(),constant()) -> 'ok' | {'error', file:posix()}.
-spec setns(alcove_drv:ref(),fork_path(),iodata(),constant(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec setopt(alcove_drv:ref(),fork_path(),atom(),non_neg_integer()) -> boolean().
-spec setopt(alcove_drv:ref(),fork_path(),atom(),non_neg_integer(),timeout()) -> boolean().

-spec setpgid(alcove_drv:ref(),fork_path(),pid_t(),pid_t()) -> 'ok' | {'error', file:posix()}.
-spec setpgid(alcove_drv:ref(),fork_path(),pid_t(),pid_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec setpriority(alcove_drv:ref(),fork_path(),constant(),int32_t(),int32_t()) -> 'ok' | {'error', file:posix() | 'unsupported'}.
-spec setpriority(alcove_drv:ref(),fork_path(),constant(),int32_t(),int32_t(),timeout()) -> 'ok' | {'error', file:posix() | 'unsupported'}.

-spec setproctitle(alcove_drv:ref(),fork_path(),iodata()) -> 'ok'.
-spec setproctitle(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> 'ok'.

-spec setresgid(alcove_drv:ref(),fork_path(),gid_t(),gid_t(),gid_t()) -> 'ok' | {'error', file:posix()}.
-spec setresgid(alcove_drv:ref(),fork_path(),gid_t(),gid_t(),gid_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec setresuid(alcove_drv:ref(),fork_path(),uid_t(),uid_t(),uid_t()) -> 'ok' | {'error', file:posix()}.
-spec setresuid(alcove_drv:ref(),fork_path(),uid_t(),uid_t(),uid_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec setrlimit(alcove_drv:ref(),fork_path(),constant(),#alcove_rlimit{}) -> 'ok' | {'error', file:posix() | 'unsupported'}.
-spec setrlimit(alcove_drv:ref(),fork_path(),constant(),#alcove_rlimit{},timeout()) -> 'ok' | {'error', file:posix() | 'unsupported'}.

-spec setsid(alcove_drv:ref(),fork_path()) -> {ok,pid_t()} | {error, file:posix()}.
-spec setsid(alcove_drv:ref(),fork_path(),timeout()) -> {ok,pid_t()} | {error, file:posix()}.

-spec setuid(alcove_drv:ref(),fork_path(),uid_t()) -> 'ok' | {'error', file:posix()}.
-spec setuid(alcove_drv:ref(),fork_path(),uid_t(),timeout()) -> 'ok' | {'error', file:posix()}.

-spec sigaction(alcove_drv:ref(),fork_path(),constant(),atom()) -> 'ok' | {'error', file:posix() | 'unsupported'}.
-spec sigaction(alcove_drv:ref(),fork_path(),constant(),atom(),timeout()) -> 'ok' | {'error', file:posix() | 'unsupported'}.

-spec signal_constant(alcove_drv:ref(),fork_path(),non_neg_integer()) -> 'unknown' | atom().
-spec signal_constant(alcove_drv:ref(),fork_path(),non_neg_integer(),timeout()) -> 'unknown' | atom().

-spec signal_define(alcove_drv:ref(),fork_path(),atom()) -> 'unknown' | non_neg_integer().
-spec signal_define(alcove_drv:ref(),fork_path(),atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec syscall_define(alcove_drv:ref(),fork_path(),atom()) -> 'unknown' | non_neg_integer().
-spec syscall_define(alcove_drv:ref(),fork_path(),atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec stderr(alcove_drv:ref(),fork_path()) -> 'false' | binary().
-spec stderr(alcove_drv:ref(),fork_path(),timeout()) -> 'false' | binary().

-spec stdin(alcove_drv:ref(),fork_path(),iodata()) -> 'true'.

-spec stdout(alcove_drv:ref(),fork_path()) -> 'false' | binary().
-spec stdout(alcove_drv:ref(),fork_path(),timeout()) -> 'false' | binary().

-spec umount(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {error, file:posix()}.
-spec umount(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> 'ok' | {error, file:posix()}.

-spec unsetenv(alcove_drv:ref(),fork_path(),iodata()) -> 'ok' | {error, file:posix()}.
-spec unsetenv(alcove_drv:ref(),fork_path(),iodata(),timeout()) -> 'ok' | {error, file:posix()}.

-spec unshare(alcove_drv:ref(),fork_path(),int32_t() | [constant()]) -> 'ok' | {'error', file:posix() | 'unsupported'}.
-spec unshare(alcove_drv:ref(),fork_path(),int32_t() | [constant()],timeout()) -> 'ok' | {'error', file:posix() | 'unsupported'}.

-spec write(alcove_drv:ref(),fork_path(),fd(),iodata()) -> {'ok', ssize_t()} | {'error', file:posix()}.
-spec write(alcove_drv:ref(),fork_path(),fd(),iodata(),timeout()) -> {'ok', ssize_t()} | {'error', file:posix()}.

-spec version(alcove_drv:ref(),fork_path()) -> binary().
-spec version(alcove_drv:ref(),fork_path(),timeout()) -> binary().
".
