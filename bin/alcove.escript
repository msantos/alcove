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
     {wordalign,1}, {wordalign,2},

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
    proplists:get_value({Arch,OS,Wordsize}, Arches, enotsup).
";

static({wordalign,1}) ->
"wordalign(Offset) ->
    wordalign(Offset, erlang:system_info({wordsize, external})).
";
static({wordalign,2}) ->
"wordalign(Offset, Align) ->
    (Align - (Offset rem Align)) rem Align.
";

static({define,3}) ->
"
define(Drv, ForkChain, Constant) when is_atom(Constant) ->
    define(Drv, ForkChain, [Constant]);
define(Drv, ForkChain, Constants) when is_list(Constants) ->
    lists:foldl(fun
            (Constant,Result) when is_atom(Constant) ->
                Val = define_constant(Drv, ForkChain, Constant),
                Result bxor Val;
            (Val,Result) when is_integer(Val) ->
                Result bxor Val
        end,
        0,
        Constants).

define_constant(Drv, ForkChain, Constant) ->
    Fun = [
        fun clone_constant/3,
        fun fcntl_constant/3,
        fun file_constant/3,
        fun ioctl_constant/3,
        fun mount_constant/3,
        fun prctl_constant/3,
        fun rlimit_constant/3,
        fun signal_constant/3,
        fun syscall_constant/3
    ],
    define_foreach(Drv, ForkChain, Constant, Fun).

define_foreach(_Drv, _ForkChain, Constant, []) ->
    erlang:error({unknown, Constant});
define_foreach(Drv, ForkChain, Constant, [Fun|Rest]) ->
    try Fun(Drv, ForkChain, Constant) of
        unknown ->
            define_foreach(Drv, ForkChain, Constant, Rest);
        Val when is_integer(Val) ->
            Val
    catch
        % Function call not supported on this platform
        error:undef ->
            define_foreach(Drv, ForkChain, Constant, Rest)
    end.
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

-type fd() :: int32_t().
-type fd_set() :: [fd()].

-type constant() :: atom() | integer().

-type cstruct() :: nonempty_list(binary() | {ptr, binary() | non_neg_integer()}).

-type posix() :: 'e2big'
    | 'eacces' | 'eaddrinuse' | 'eaddrnotavail' | 'eadv' | 'eafnosupport'
    | 'eagain' | 'ealign' | 'ealready'
    | 'ebade' | 'ebadf' | 'ebadfd' | 'ebadmsg' | 'ebadr' | 'ebadrpc'
    | 'ebadrqc' | 'ebadslt' | 'ebfont' | 'ebusy'
    | 'ecapmode' | 'echild' | 'echrng' | 'ecomm' | 'econnaborted'
    | 'econnrefused' | 'econnreset'
    | 'edeadlk' | 'edeadlock' | 'edestaddrreq' | 'edirty' | 'edom' | 'edotdot'
    | 'edquot' | 'eduppkg'
    | 'eexist'
    | 'efault' | 'efbig'
    | 'ehostdown' | 'ehostunreach'
    | 'eidrm' | 'einit' | 'einprogress' | 'eintr' | 'einval' | 'eio'
    | 'eisconn' | 'eisdir' | 'eisnam'
    | 'el2hlt' | 'el2nsync' | 'el3hlt' | 'el3rst' | 'elbin' | 'elibacc'
    | 'elibbad' | 'elibexec' | 'elibmax' | 'elibscn' | 'elnrng' | 'eloop'
    | 'emfile' | 'emlink' | 'emsgsize' | 'emultihop'
    | 'enametoolong' | 'enavail' | 'enet' | 'enetdown' | 'enetreset'
    | 'enetunreach' | 'enfile' | 'enoano' | 'enobufs' | 'enocsi' | 'enodata'
    | 'enodev' | 'enoent' | 'enoexec' | 'enolck' | 'enolink' | 'enomem'
    | 'enomsg' | 'enonet' | 'enopkg' | 'enoprotoopt' | 'enospc' | 'enosr'
    | 'enostr' | 'enosym' | 'enosys' | 'enotblk' | 'enotcapable' | 'enotconn'
    | 'enotdir' | 'enotempty' | 'enotnam' | 'enotrecoverable' | 'enotsock'
    | 'enotsup' | 'enotty' | 'enotuniq' | 'enxio' | 'eopnotsupp'
    | 'eoverflow' | 'eownerdead'
    | 'eperm' | 'epfnosupport' | 'epipe' | 'eproclim' | 'eprocunavail'
    | 'eprogmismatch' | 'eprogunavail' | 'eproto' | 'eprotonosupport'
    | 'eprototype'
    | 'erange' | 'erefused' | 'eremchg' | 'eremdev' | 'eremote' | 'eremoteio'
    | 'eremoterelease' | 'erofs' | 'erpcmismatch' | 'erremote'
    | 'eshutdown' | 'esocktnosupport' | 'espipe' | 'esrch' | 'esrmnt'
    | 'estale' | 'esuccess'
    | 'etime' | 'etimedout' | 'etoomanyrefs' | 'etxtbsy'
    | 'euclean' | 'eunatch' | 'eusers'
    | 'eversion'
    | 'ewouldblock'
    | 'exdev' | 'exfull'.

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
        constant/0,

        posix/0
    ]).

-spec audit_arch() -> atom().

-spec call(alcove_drv:ref(),[pid_t()],atom(),list()) -> term().
-spec call(alcove_drv:ref(),[pid_t()],atom(),list(),timeout()) -> term().

-spec cap_constant(alcove_drv:ref(),[pid_t()],atom()) -> integer() | 'unknown'.
-spec cap_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> integer() | 'unknown'.

-spec cap_enter(alcove_drv:ref(),[pid_t()]) -> 'ok' | {'error', posix()}.
-spec cap_enter(alcove_drv:ref(),[pid_t()],timeout()) -> 'ok' | {'error', posix()}.

-spec cap_fcntls_limit(alcove_drv:ref(),[pid_t()],fd(),constant()) -> 'ok' | {'error', posix()}.
-spec cap_fcntls_limit(alcove_drv:ref(),[pid_t()],fd(),constant(),timeout()) -> 'ok' | {'error', posix()}.

-spec cap_getmode(alcove_drv:ref(),[pid_t()]) -> {'ok', 0 | 1} | {'error', posix()}.
-spec cap_getmode(alcove_drv:ref(),[pid_t()],timeout()) -> {'ok', 0 | 1} | {'error', posix()}.

-spec cap_ioctls_limit(alcove_drv:ref(),[pid_t()],fd(),constant()) -> 'ok' | {'error', posix()}.
-spec cap_ioctls_limit(alcove_drv:ref(),[pid_t()],fd(),constant(),timeout()) -> 'ok' | {'error', posix()}.

-spec cap_rights_limit(alcove_drv:ref(),[pid_t()],fd(),constant()) -> 'ok' | {'error', posix()}.
-spec cap_rights_limit(alcove_drv:ref(),[pid_t()],fd(),constant(),timeout()) -> 'ok' | {'error', posix()}.

-spec chdir(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {'error', posix()}.
-spec chdir(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec chmod(alcove_drv:ref(),[pid_t()],iodata(),mode_t()) -> 'ok' | {'error', posix()}.
-spec chmod(alcove_drv:ref(),[pid_t()],iodata(),mode_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec chown(alcove_drv:ref(),[pid_t()],iodata(),uid_t(),gid_t()) -> 'ok' | {'error', posix()}.
-spec chown(alcove_drv:ref(),[pid_t()],iodata(),uid_t(),gid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec chroot(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {'error', posix()}.
-spec chroot(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec clearenv(alcove_drv:ref(),[pid_t()]) -> 'ok' | {'error', posix()}.
-spec clearenv(alcove_drv:ref(),[pid_t()],timeout()) -> 'ok' | {'error', posix()}.

-spec clone(alcove_drv:ref(),[pid_t()],int32_t() | [constant()]) -> {'ok', pid_t()} | {'error', posix()}.
-spec clone(alcove_drv:ref(),[pid_t()],int32_t() | [constant()],timeout()) -> {'ok', pid_t()} | {'error', posix()}.

-spec clone_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | int32_t().
-spec clone_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | int32_t().

-spec close(alcove_drv:ref(),[pid_t()],fd()) -> 'ok' | {'error', posix()}.
-spec close(alcove_drv:ref(),[pid_t()],fd(),timeout()) -> 'ok' | {'error', posix()}.

-spec define(alcove_drv:ref(),[pid_t()],atom() | [atom()]) -> integer().

-spec environ(alcove_drv:ref(),[pid_t()]) -> [binary()].
-spec environ(alcove_drv:ref(),[pid_t()],timeout()) -> [binary()].

-spec eof(alcove_drv:ref(),[pid_t()],'stdin' | 'stdout' | 'stderr') -> 'ok' | {'error',posix()}.

-spec errno_id(alcove_drv:ref(),[pid_t()],int32_t()) -> posix().
-spec errno_id(alcove_drv:ref(),[pid_t()],int32_t(),timeout()) -> posix().

-spec event(alcove_drv:ref(),[pid_t()]) -> term().
-spec event(alcove_drv:ref(),[pid_t()],timeout()) -> term().

-spec execve(alcove_drv:ref(),[pid_t()],iodata(),[iodata()],[iodata()]) -> 'ok' | {'error',posix()}.
-spec execve(alcove_drv:ref(),[pid_t()],iodata(),[iodata()],[iodata()],timeout()) -> 'ok' | {'error',posix()}.

-spec execvp(alcove_drv:ref(),[pid_t()],iodata(),[iodata()]) -> 'ok' | {'error',posix()}.
-spec execvp(alcove_drv:ref(),[pid_t()],iodata(),[iodata()],timeout()) -> 'ok' | {'error',posix()}.

-spec exit(alcove_drv:ref(),[pid_t()],int32_t()) -> 'ok'.
-spec exit(alcove_drv:ref(),[pid_t()],int32_t(),timeout()) -> 'ok'.

-spec fcntl(alcove_drv:ref(), [pid_t()], fd(), constant(), int64_t()) -> {'ok',int64_t()} | {'error', posix()}.
-spec fcntl(alcove_drv:ref(), [pid_t()], fd(), constant(), int64_t(), timeout()) -> {'ok',int64_t()} | {'error', posix()}.

-spec file_constant(alcove_drv:ref(),[pid_t()],atom()) -> non_neg_integer() | 'unknown'.
-spec file_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> non_neg_integer() | 'unknown'.

-spec fork(alcove_drv:ref(),[pid_t()]) -> {'ok', pid_t()} | {'error', posix()}.
-spec fork(alcove_drv:ref(),[pid_t()],timeout()) -> {'ok', pid_t()} | {'error', posix()}.

-spec getcwd(alcove_drv:ref(),[pid_t()]) -> {'ok', binary()} | {'error', posix()}.
-spec getcwd(alcove_drv:ref(),[pid_t()],timeout()) -> {'ok', binary()} | {'error', posix()}.

-spec getenv(alcove_drv:ref(),[pid_t()],iodata()) -> binary() | 'false'.
-spec getenv(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> binary() | 'false'.

-spec getgid(alcove_drv:ref(),[pid_t()]) -> gid_t().
-spec getgid(alcove_drv:ref(),[pid_t()],timeout()) -> gid_t().

-spec getgroups(alcove_drv:ref(),[pid_t()]) -> {ok, [gid_t()]} | {error, posix()}.
-spec getgroups(alcove_drv:ref(),[pid_t()],timeout()) -> {ok, [gid_t()]} | {error, posix()}.

-spec gethostname(alcove_drv:ref(),[pid_t()]) -> {'ok', binary()} | {'error', posix()}.
-spec gethostname(alcove_drv:ref(),[pid_t()],timeout()) -> {'ok', binary()} | {'error', posix()}.

-spec getopt(alcove_drv:ref(),[pid_t()],atom()) -> 'false' | non_neg_integer().
-spec getopt(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'false' | non_neg_integer().

-spec getpgrp(alcove_drv:ref(),[pid_t()]) -> pid_t().
-spec getpgrp(alcove_drv:ref(),[pid_t()],timeout()) -> pid_t().

-spec getpid(alcove_drv:ref(),[pid_t()]) -> pid_t().
-spec getpid(alcove_drv:ref(),[pid_t()],timeout()) -> pid_t().

-spec getpriority(alcove_drv:ref(),[pid_t()],constant(),int32_t()) -> {'ok',int32_t()} | {'error', posix()}.
-spec getpriority(alcove_drv:ref(),[pid_t()],constant(),int32_t(),timeout()) -> {'ok',int32_t()} | {'error', posix()}.

-spec getresgid(alcove_drv:ref(),[pid_t()]) -> {'ok', gid_t(), gid_t(), gid_t()} | {'error', posix()}.
-spec getresgid(alcove_drv:ref(),[pid_t()],timeout()) -> {'ok', gid_t(), gid_t(), gid_t()} | {'error', posix()}.

-spec getresuid(alcove_drv:ref(),[pid_t()]) -> {'ok', uid_t(), uid_t(), uid_t()} | {'error', posix()}.
-spec getresuid(alcove_drv:ref(),[pid_t()],timeout()) -> {'ok', uid_t(), uid_t(), uid_t()} | {'error', posix()}.

-spec getrlimit(alcove_drv:ref(),[pid_t()],constant()) -> {'ok', #alcove_rlimit{}} | {'error', posix()}.
-spec getrlimit(alcove_drv:ref(),[pid_t()],constant(),timeout()) -> {'ok', #alcove_rlimit{}} | {'error', posix()}.

-spec getsid(alcove_drv:ref(),[pid_t()],pid_t()) -> {'ok', pid_t()} | {'error', posix()}.
-spec getsid(alcove_drv:ref(),[pid_t()],pid_t(),timeout()) -> {'ok', pid_t()} | {'error', posix()}.

-spec getuid(alcove_drv:ref(),[pid_t()]) -> uid_t().
-spec getuid(alcove_drv:ref(),[pid_t()],timeout()) -> uid_t().

-spec ioctl(alcove_drv:ref(), [pid_t()], fd(), constant(), cstruct()) -> {'ok',iodata()} | {'error', posix()}.
-spec ioctl(alcove_drv:ref(), [pid_t()], fd(), constant(), cstruct(), timeout()) -> {'ok',iodata()} | {'error', posix()}.

-spec jail(alcove_drv:ref(),[pid_t()],cstruct()) -> {'ok', int32_t()} | {'error', posix()}.
-spec jail(alcove_drv:ref(),[pid_t()],cstruct(),timeout()) -> {'ok', int32_t()} | {'error', posix()}.

-spec jail_attach(alcove_drv:ref(),[pid_t()],int32_t()) -> 'ok' | {'error', posix()}.
-spec jail_attach(alcove_drv:ref(),[pid_t()],int32_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec jail_remove(alcove_drv:ref(),[pid_t()],int32_t()) -> 'ok' | {'error', posix()}.
-spec jail_remove(alcove_drv:ref(),[pid_t()],int32_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec kill(alcove_drv:ref(),[pid_t()],pid_t(),constant()) -> 'ok' | {'error', posix()}.
-spec kill(alcove_drv:ref(),[pid_t()],pid_t(),constant(),timeout()) -> 'ok' | {'error', posix()}.

-spec link(alcove_drv:ref(),[pid_t()],iodata(),iodata()) -> 'ok' | {error, posix()}.
-spec link(alcove_drv:ref(),[pid_t()],iodata(),iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec lseek(alcove_drv:ref(),[pid_t()],fd(),off_t(),int32_t()) -> 'ok' | {'error', posix()}.
-spec lseek(alcove_drv:ref(),[pid_t()],fd(),off_t(),int32_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec mkdir(alcove_drv:ref(),[pid_t()],iodata(),mode_t()) -> 'ok' | {'error', posix()}.
-spec mkdir(alcove_drv:ref(),[pid_t()],iodata(),mode_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec mkfifo(alcove_drv:ref(),[pid_t()],iodata(),mode_t()) -> 'ok' | {'error', posix()}.
-spec mkfifo(alcove_drv:ref(),[pid_t()],iodata(),mode_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec mount(alcove_drv:ref(),[pid_t()],iodata(),iodata(),iodata(),uint64_t() | [constant()],iodata(),iodata()) -> 'ok' | {'error', posix()}.
-spec mount(alcove_drv:ref(),[pid_t()],iodata(),iodata(),iodata(),uint64_t() | [constant()],iodata(),iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec mount_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | uint64_t().
-spec mount_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | uint64_t().

-spec open(alcove_drv:ref(),[pid_t()],iodata(),int32_t() | [constant()],mode_t()) -> {'ok',fd()} | {'error', posix()}.
-spec open(alcove_drv:ref(),[pid_t()],iodata(),int32_t() | [constant()],mode_t(),timeout()) -> {'ok',fd()} | {'error', posix()}.

-spec pid(alcove_drv:ref(),[pid_t()]) -> [#alcove_pid{}].
-spec pid(alcove_drv:ref(),[pid_t()],timeout()) -> [#alcove_pid{}].

-spec pivot_root(alcove_drv:ref(),[pid_t()],iodata(),iodata()) -> 'ok' | {'error', posix()}.
-spec pivot_root(alcove_drv:ref(),[pid_t()],iodata(),iodata(),timeout()) -> 'ok' | {'error', posix()}.

-type prctl_arg() :: binary() | constant() | cstruct().
-type prctl_val() :: binary() | integer() | cstruct().
-spec prctl(alcove_drv:ref(),[pid_t()],constant(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg())
    -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()} | {'error', posix()}.
-spec prctl(alcove_drv:ref(),[pid_t()],constant(),prctl_arg(),prctl_arg(),prctl_arg(),prctl_arg(),timeout())
    -> {'ok',integer(),prctl_val(),prctl_val(),prctl_val(),prctl_val()} | {'error', posix()}.

-spec prctl_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec prctl_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec read(alcove_drv:ref(),[pid_t()],fd(),size_t()) -> {'ok', binary()} | {'error', posix()}.
-spec read(alcove_drv:ref(),[pid_t()],fd(),size_t(),timeout()) -> {'ok', binary()} | {'error', posix()}.

-spec readdir(alcove_drv:ref(),[pid_t()],iodata()) -> {'ok', [binary()]} | {'error', posix()}.
-spec readdir(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> {'ok', [binary()]} | {'error', posix()}.

-spec rmdir(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {'error', posix()}.
-spec rmdir(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec rlimit_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec rlimit_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec select(alcove_drv:ref(),[pid_t()],fd_set(),fd_set(),fd_set(),
    <<>> | #alcove_timeval{}) -> {ok, fd_set(), fd_set(), fd_set()} | {'error', posix()}.
-spec select(alcove_drv:ref(),[pid_t()],fd_set(),fd_set(),fd_set(),
    <<>> | #alcove_timeval{},timeout()) -> {ok, fd_set(), fd_set(), fd_set()} | {'error', posix()}.

-spec setenv(alcove_drv:ref(),[pid_t()],iodata(),iodata(),int32_t()) -> 'ok' | {'error', posix()}.
-spec setenv(alcove_drv:ref(),[pid_t()],iodata(),iodata(),int32_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setgid(alcove_drv:ref(),[pid_t()],gid_t()) -> 'ok' | {'error', posix()}.
-spec setgid(alcove_drv:ref(),[pid_t()],gid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setgroups(alcove_drv:ref(),[pid_t()],[gid_t()]) -> 'ok' | {'error', posix()}.
-spec setgroups(alcove_drv:ref(),[pid_t()],[gid_t()],timeout()) -> 'ok' | {'error', posix()}.

-spec sethostname(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {'error', posix()}.
-spec sethostname(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec setns(alcove_drv:ref(),[pid_t()],iodata(),constant()) -> 'ok' | {'error', posix()}.
-spec setns(alcove_drv:ref(),[pid_t()],iodata(),constant(),timeout()) -> 'ok' | {'error', posix()}.

-spec setopt(alcove_drv:ref(),[pid_t()],atom(),non_neg_integer()) -> boolean().
-spec setopt(alcove_drv:ref(),[pid_t()],atom(),non_neg_integer(),timeout()) -> boolean().

-spec setpgid(alcove_drv:ref(),[pid_t()],pid_t(),pid_t()) -> 'ok' | {'error', posix()}.
-spec setpgid(alcove_drv:ref(),[pid_t()],pid_t(),pid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setpriority(alcove_drv:ref(),[pid_t()],constant(),int32_t(),int32_t()) -> 'ok' | {'error', posix()}.
-spec setpriority(alcove_drv:ref(),[pid_t()],constant(),int32_t(),int32_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setproctitle(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok'.
-spec setproctitle(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok'.

-spec setresgid(alcove_drv:ref(),[pid_t()],gid_t(),gid_t(),gid_t()) -> 'ok' | {'error', posix()}.
-spec setresgid(alcove_drv:ref(),[pid_t()],gid_t(),gid_t(),gid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setresuid(alcove_drv:ref(),[pid_t()],uid_t(),uid_t(),uid_t()) -> 'ok' | {'error', posix()}.
-spec setresuid(alcove_drv:ref(),[pid_t()],uid_t(),uid_t(),uid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setrlimit(alcove_drv:ref(),[pid_t()],constant(),#alcove_rlimit{}) -> 'ok' | {'error', posix()}.
-spec setrlimit(alcove_drv:ref(),[pid_t()],constant(),#alcove_rlimit{},timeout()) -> 'ok' | {'error', posix()}.

-spec setsid(alcove_drv:ref(),[pid_t()]) -> {ok,pid_t()} | {error, posix()}.
-spec setsid(alcove_drv:ref(),[pid_t()],timeout()) -> {ok,pid_t()} | {error, posix()}.

-spec setuid(alcove_drv:ref(),[pid_t()],uid_t()) -> 'ok' | {'error', posix()}.
-spec setuid(alcove_drv:ref(),[pid_t()],uid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec sigaction(alcove_drv:ref(),[pid_t()],constant(),atom()) -> {'ok',atom()} | {'error', posix()}.
-spec sigaction(alcove_drv:ref(),[pid_t()],constant(),atom(),timeout()) -> {'ok',atom()} | {'error', posix()}.

-spec signal_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec signal_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec syscall_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec syscall_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec stderr(alcove_drv:ref(),[pid_t()]) -> 'false' | binary().
-spec stderr(alcove_drv:ref(),[pid_t()],timeout()) -> 'false' | binary().

-spec stdin(alcove_drv:ref(),[pid_t()],iodata()) -> 'true'.

-spec stdout(alcove_drv:ref(),[pid_t()]) -> 'false' | binary().
-spec stdout(alcove_drv:ref(),[pid_t()],timeout()) -> 'false' | binary().

-spec symlink(alcove_drv:ref(),[pid_t()],iodata(),iodata()) -> 'ok' | {error, posix()}.
-spec symlink(alcove_drv:ref(),[pid_t()],iodata(),iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec unlink(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {error, posix()}.
-spec unlink(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec umount(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {error, posix()}.
-spec umount(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec unsetenv(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {error, posix()}.
-spec unsetenv(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec unshare(alcove_drv:ref(),[pid_t()],int32_t() | [constant()]) -> 'ok' | {'error', posix()}.
-spec unshare(alcove_drv:ref(),[pid_t()],int32_t() | [constant()],timeout()) -> 'ok' | {'error', posix()}.

-spec write(alcove_drv:ref(),[pid_t()],fd(),iodata()) -> {'ok', ssize_t()} | {'error', posix()}.
-spec write(alcove_drv:ref(),[pid_t()],fd(),iodata(),timeout()) -> {'ok', ssize_t()} | {'error', posix()}.

-spec version(alcove_drv:ref(),[pid_t()]) -> binary().
-spec version(alcove_drv:ref(),[pid_t()],timeout()) -> binary().
".
