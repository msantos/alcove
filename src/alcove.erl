% Copyright (c) 2021, Michael Santos <michael.santos@gmail.com>
% Permission to use, copy, modify, and/or distribute this software for any
% purpose with or without fee is hereby granted, provided that the above
% copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%% % @noformat

-module(alcove).

-include("alcove.hrl").


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

-type alcove_pid_field() :: pid
    | flowcontrol
    | signaloneof
    | fdctl
    | stdin
    | stdout
    | stderr.

-type alcove_pid() :: #alcove_pid{}.
-type alcove_rlimit() :: #alcove_rlimit{}.
-type alcove_timeval() :: #alcove_timeval{}.

-type filter() :: alcove_proto:calls() | []
  | {'deny', alcove_proto:calls() | []}
  | {'allow', alcove_proto:calls() | []}.

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

        posix/0,

        filter/0,

        alcove_pid_field/0,
        alcove_pid/0,
        alcove_rlimit/0,
        alcove_timeval/0
    ]).

-spec audit_arch() -> atom().

-spec cap_constant(alcove_drv:ref(),[pid_t()],atom()) -> integer() | 'unknown'.
-spec cap_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> integer() | 'unknown'.

-spec cap_enter(alcove_drv:ref(),[pid_t()]) -> 'ok' | {'error', posix()}.
-spec cap_enter(alcove_drv:ref(),[pid_t()],timeout()) -> 'ok' | {'error', posix()}.

-spec cap_fcntls_get(alcove_drv:ref(),[pid_t()],fd()) -> {'ok', integer()} | {'error', posix()}.
-spec cap_fcntls_get(alcove_drv:ref(),[pid_t()],fd(),timeout()) -> {'ok', integer()} | {'error', posix()}.

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

-spec cpid(alcove_drv:ref(),[pid_t()]) -> [alcove_pid()].
-spec cpid(alcove_drv:ref(),[pid_t()],timeout()) -> [alcove_pid()].

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

-spec connect(alcove_drv:ref(),[pid_t()],fd(), [] | cstruct()) -> 'ok' | {'error', posix()}.
-spec connect(alcove_drv:ref(),[pid_t()],fd(), [] | cstruct(),timeout()) -> 'ok' | {'error', posix()}.

-spec define(alcove_drv:ref(),[pid_t()],atom() | [atom()]) -> integer().

-spec environ(alcove_drv:ref(),[pid_t()]) -> [binary()].
-spec environ(alcove_drv:ref(),[pid_t()],timeout()) -> [binary()].

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

-spec fcntl_constant(alcove_drv:ref(),[pid_t()],atom()) -> integer() | 'unknown'.
-spec fcntl_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> integer() | 'unknown'.

-spec fexecve(alcove_drv:ref(),[pid_t()],fd(),[iodata()],[iodata()]) -> 'ok' | {'error',posix()}.
-spec fexecve(alcove_drv:ref(),[pid_t()],fd(),[iodata()],[iodata()],timeout()) -> 'ok' | {'error',posix()}.

-spec file_constant(alcove_drv:ref(),[pid_t()],atom()) -> non_neg_integer() | 'unknown'.
-spec file_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> non_neg_integer() | 'unknown'.

-spec filter(filter()) -> [uint8_t()].

-spec filter(alcove_drv:ref(),[pid_t()],[alcove_proto:call()]) -> ok | {'error', 'einval'}.
-spec filter(alcove_drv:ref(),[pid_t()],[alcove_proto:call()],[alcove_proto:call()]) -> ok | {'error', 'einval'}.
-spec filter(alcove_drv:ref(),[pid_t()],[alcove_proto:call()],[alcove_proto:call()],timeout()) -> ok | {'error', 'einval'}.

-spec fork(alcove_drv:ref(),[pid_t()]) -> {'ok', pid_t()} | {'error', posix()}.
-spec fork(alcove_drv:ref(),[pid_t()],timeout()) -> {'ok', pid_t()} | {'error', posix()}.

-spec getcpid(alcove_drv:ref(),[pid_t()],pid_t(),alcove_pid_field()) -> 'false' | int32_t().

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

-spec getopt(alcove_drv:ref(),[pid_t()],atom()) -> 'false' | int32_t().
-spec getopt(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'false' | int32_t().

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

-spec getrlimit(alcove_drv:ref(),[pid_t()],constant()) -> {'ok', alcove_rlimit()} | {'error', posix()}.
-spec getrlimit(alcove_drv:ref(),[pid_t()],constant(),timeout()) -> {'ok', alcove_rlimit()} | {'error', posix()}.

-spec getsid(alcove_drv:ref(),[pid_t()],pid_t()) -> {'ok', pid_t()} | {'error', posix()}.
-spec getsid(alcove_drv:ref(),[pid_t()],pid_t(),timeout()) -> {'ok', pid_t()} | {'error', posix()}.

-spec getuid(alcove_drv:ref(),[pid_t()]) -> uid_t().
-spec getuid(alcove_drv:ref(),[pid_t()],timeout()) -> uid_t().

-spec ioctl(alcove_drv:ref(), [pid_t()], fd(), constant(), cstruct()) -> {'ok',integer(),iodata()} | {'error', posix()}.
-spec ioctl(alcove_drv:ref(), [pid_t()], fd(), constant(), cstruct(), timeout()) -> {'ok',integer(),iodata()} | {'error', posix()}.

-spec ioctl_constant(alcove_drv:ref(),[pid_t()],atom()) -> integer() | 'unknown'.
-spec ioctl_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> integer() | 'unknown'.

-spec iolist_to_bin(alcove_drv:ref(),[pid_t()],iodata()) -> binary().
-spec iolist_to_bin(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> binary().

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

-spec pledge(alcove_drv:ref(),[pid_t()],iodata(),iodata()) -> 'ok' | {'error', posix()}.
-spec pledge(alcove_drv:ref(),[pid_t()],iodata(),iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec pivot_root(alcove_drv:ref(),[pid_t()],iodata(),iodata()) -> 'ok' | {'error', posix()}.
-spec pivot_root(alcove_drv:ref(),[pid_t()],iodata(),iodata(),timeout()) -> 'ok' | {'error', posix()}.

-type ptr_arg() :: binary() | constant() | cstruct().
-type ptr_val() :: binary() | integer() | cstruct().
-spec prctl(alcove_drv:ref(),[pid_t()],constant(),ptr_arg(),ptr_arg(),ptr_arg(),ptr_arg())
    -> {'ok',integer(),ptr_val(),ptr_val(),ptr_val(),ptr_val()} | {'error', posix()}.
-spec prctl(alcove_drv:ref(),[pid_t()],constant(),ptr_arg(),ptr_arg(),ptr_arg(),ptr_arg(),timeout())
    -> {'ok',integer(),ptr_val(),ptr_val(),ptr_val(),ptr_val()} | {'error', posix()}.

-spec prctl_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec prctl_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec procctl(alcove_drv:ref(),[pid_t()],constant(),pid_t(),constant(),[] | cstruct())
    -> {'ok',binary(),cstruct()} | {'error', posix()}.
-spec procctl(alcove_drv:ref(),[pid_t()],constant(),pid_t(),constant(),[] | cstruct(),timeout())
    -> {'ok',binary(),cstruct()} | {'error', posix()}.

-spec ptrace(alcove_drv:ref(),[pid_t()],constant(),pid_t(),ptr_arg(),ptr_arg())
    -> {'ok', integer(), ptr_val(), ptr_val()} | {'error', posix()}.
-spec ptrace(alcove_drv:ref(),[pid_t()],constant(),pid_t(),ptr_arg(),ptr_arg(),timeout())
    -> {'ok', integer(), ptr_val(), ptr_val()} | {'error', posix()}.

-spec ptrace_constant(alcove_drv:ref(),[pid_t()],atom())
    -> 'unknown' | integer().
-spec ptrace_constant(alcove_drv:ref(),[pid_t()],atom(),timeout())
    -> 'unknown' | integer().

-spec read(alcove_drv:ref(),[pid_t()],fd(),size_t()) -> {'ok', binary()} | {'error', posix()}.
-spec read(alcove_drv:ref(),[pid_t()],fd(),size_t(),timeout()) -> {'ok', binary()} | {'error', posix()}.

-spec readdir(alcove_drv:ref(),[pid_t()],iodata()) -> {'ok', [binary()]} | {'error', posix()}.
-spec readdir(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> {'ok', [binary()]} | {'error', posix()}.

-spec rmdir(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {'error', posix()}.
-spec rmdir(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec rlimit_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec rlimit_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec seccomp(alcove_drv:ref(),[pid_t()],constant(),constant(),cstruct()) -> 'ok' | {'error', posix()}.
-spec seccomp(alcove_drv:ref(),[pid_t()],constant(),constant(),cstruct(),timeout()) -> 'ok' | {'error', posix()}.

-spec seccomp_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec seccomp_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec select(alcove_drv:ref(),[pid_t()],fd_set(),fd_set(),fd_set(),
    [] | 'null' | alcove_timeval()) -> {ok, fd_set(), fd_set(), fd_set()} | {'error', posix()}.
-spec select(alcove_drv:ref(),[pid_t()],fd_set(),fd_set(),fd_set(),
    [] | 'null' | alcove_timeval(),timeout()) -> {ok, fd_set(), fd_set(), fd_set()} | {'error', posix()}.

-spec setenv(alcove_drv:ref(),[pid_t()],iodata(),iodata(),int32_t()) -> 'ok' | {'error', posix()}.
-spec setenv(alcove_drv:ref(),[pid_t()],iodata(),iodata(),int32_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setcpid(alcove_drv:ref(),[pid_t()],pid_t(),alcove_pid_field(),int32_t()) -> boolean().
-spec setcpid(alcove_drv:ref(),[pid_t()],pid_t(),alcove_pid_field(),int32_t(),timeout()) -> boolean().

-spec setgid(alcove_drv:ref(),[pid_t()],gid_t()) -> 'ok' | {'error', posix()}.
-spec setgid(alcove_drv:ref(),[pid_t()],gid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec setgroups(alcove_drv:ref(),[pid_t()],[gid_t()]) -> 'ok' | {'error', posix()}.
-spec setgroups(alcove_drv:ref(),[pid_t()],[gid_t()],timeout()) -> 'ok' | {'error', posix()}.

-spec sethostname(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {'error', posix()}.
-spec sethostname(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {'error', posix()}.

-spec setns(alcove_drv:ref(),[pid_t()],iodata(),constant()) -> 'ok' | {'error', posix()}.
-spec setns(alcove_drv:ref(),[pid_t()],iodata(),constant(),timeout()) -> 'ok' | {'error', posix()}.

-spec setopt(alcove_drv:ref(),[pid_t()],atom(),int32_t()) -> boolean().
-spec setopt(alcove_drv:ref(),[pid_t()],atom(),int32_t(),timeout()) -> boolean().

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

-spec setrlimit(alcove_drv:ref(),[pid_t()],constant(),alcove_rlimit()) -> 'ok' | {'error', posix()}.
-spec setrlimit(alcove_drv:ref(),[pid_t()],constant(),alcove_rlimit(),timeout()) -> 'ok' | {'error', posix()}.

-spec setsid(alcove_drv:ref(),[pid_t()]) -> {ok,pid_t()} | {error, posix()}.
-spec setsid(alcove_drv:ref(),[pid_t()],timeout()) -> {ok,pid_t()} | {error, posix()}.

-spec setuid(alcove_drv:ref(),[pid_t()],uid_t()) -> 'ok' | {'error', posix()}.
-spec setuid(alcove_drv:ref(),[pid_t()],uid_t(),timeout()) -> 'ok' | {'error', posix()}.

-spec sigaction(alcove_drv:ref(),[pid_t()],constant() | [] | <<>>,atom()) -> {'ok',atom()} | {'error', posix()}.
-spec sigaction(alcove_drv:ref(),[pid_t()],constant() | [] | <<>>,atom(),timeout()) -> {'ok',atom()} | {'error', posix()}.

-spec signal_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec signal_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec socket(alcove_drv:ref(),[pid_t()],constant(),constant(),int32_t()) -> {'ok',fd()} | {'error', posix()}.
-spec socket(alcove_drv:ref(),[pid_t()],constant(),constant(),int32_t(),timeout()) -> {'ok',fd()} | {'error', posix()}.

-spec syscall_constant(alcove_drv:ref(),[pid_t()],atom()) -> 'unknown' | non_neg_integer().
-spec syscall_constant(alcove_drv:ref(),[pid_t()],atom(),timeout()) -> 'unknown' | non_neg_integer().

-spec stderr(alcove_drv:ref(),[pid_t()]) -> [binary()].
-spec stderr(alcove_drv:ref(),[pid_t()],timeout()) -> [binary()].

-spec stdin(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok'.

-spec stdout(alcove_drv:ref(),[pid_t()]) -> [binary()].
-spec stdout(alcove_drv:ref(),[pid_t()],timeout()) -> [binary()].

-spec symlink(alcove_drv:ref(),[pid_t()],iodata(),iodata()) -> 'ok' | {error, posix()}.
-spec symlink(alcove_drv:ref(),[pid_t()],iodata(),iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec unlink(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {error, posix()}.
-spec unlink(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec umount(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {error, posix()}.
-spec umount(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec umount2(alcove_drv:ref(),[pid_t()],iodata(),int32_t() | [constant()]) -> 'ok' | {error, posix()}.
-spec umount2(alcove_drv:ref(),[pid_t()],iodata(),int32_t() | [constant()],timeout()) -> 'ok' | {error, posix()}.

-spec unsetenv(alcove_drv:ref(),[pid_t()],iodata()) -> 'ok' | {error, posix()}.
-spec unsetenv(alcove_drv:ref(),[pid_t()],iodata(),timeout()) -> 'ok' | {error, posix()}.

-spec unshare(alcove_drv:ref(),[pid_t()],int32_t() | [constant()]) -> 'ok' | {'error', posix()}.
-spec unshare(alcove_drv:ref(),[pid_t()],int32_t() | [constant()],timeout()) -> 'ok' | {'error', posix()}.

-spec unveil(alcove_drv:ref(),[pid_t()],iodata(),iodata()) -> 'ok' | {'error', posix()}.
-spec unveil(alcove_drv:ref(),[pid_t()],iodata(),iodata(),timeout()) -> 'ok' | {'error', posix()}.

-type waitpid_value() :: {exit_status, int32_t()}
    | {termsig, atom()}
    | {stopsig, atom()}
    | continued.
-spec waitpid(alcove_drv:ref(),[pid_t()],pid_t(),int32_t() | [constant()]) -> {'ok', pid_t(), [waitpid_value()]} | {'error', posix()}.
-spec waitpid(alcove_drv:ref(),[pid_t()],pid_t(),int32_t() | [constant()],timeout()) -> {'ok', pid_t(), [waitpid_value()]} | {'error', posix()}.

-spec write(alcove_drv:ref(),[pid_t()],fd(),iodata()) -> {'ok', ssize_t()} | {'error', posix()}.
-spec write(alcove_drv:ref(),[pid_t()],fd(),iodata(),timeout()) -> {'ok', ssize_t()} | {'error', posix()}.

-spec version(alcove_drv:ref(),[pid_t()]) -> binary().
-spec version(alcove_drv:ref(),[pid_t()],timeout()) -> binary().


% Static functions

-export([audit_arch/0,
         wordalign/1,
         wordalign/2,
         define/3,
         stdin/3,
         stdout/2,
         stdout/3,
         stderr/2,
         stderr/3,
         eof/2,
         eof/3,
         event/2,
         event/3,
         filter/1,
         filter/3,
         getcpid/4]).

% Generated functions

-export([alloc/3,
         cap_constant/3,
         cap_enter/2,
         cap_fcntls_get/3,
         cap_fcntls_limit/4,
         cap_getmode/2,
         cap_ioctls_limit/4,
         cap_rights_limit/4,
         chdir/3,
         chmod/4,
         chown/5,
         chroot/3,
         clearenv/2,
         clone/3,
         clone_constant/3,
         close/3,
         connect/4,
         cpid/2,
         environ/2,
         errno_id/3,
         execve/5,
         execvp/4,
         exit/3,
         fcntl/5,
         fcntl_constant/3,
         fexecve/5,
         file_constant/3,
         filter/4,
         fork/2,
         getcwd/2,
         getenv/3,
         getgid/2,
         getgroups/2,
         gethostname/2,
         getopt/3,
         getpgrp/2,
         getpid/2,
         getpriority/4,
         getresgid/2,
         getresuid/2,
         getrlimit/3,
         getsid/3,
         getuid/2,
         ioctl/5,
         ioctl_constant/3,
         iolist_to_bin/3,
         jail/3,
         jail_attach/3,
         jail_remove/3,
         kill/4,
         link/4,
         lseek/5,
         mkdir/4,
         mkfifo/4,
         mount/8,
         mount_constant/3,
         open/5,
         pivot_root/4,
         pledge/4,
         prctl/7,
         prctl_constant/3,
         procctl/6,
         ptrace/6,
         ptrace_constant/3,
         read/4,
         readdir/3,
         rlimit_constant/3,
         rmdir/3,
         seccomp/5,
         seccomp_constant/3,
         select/6,
         setcpid/5,
         setenv/5,
         setgid/3,
         setgroups/3,
         sethostname/3,
         setns/4,
         setopt/4,
         setpgid/4,
         setpriority/5,
         setproctitle/3,
         setresgid/5,
         setresuid/5,
         setrlimit/4,
         setsid/2,
         setuid/3,
         sigaction/4,
         signal_constant/3,
         socket/5,
         symlink/4,
         syscall_constant/3,
         umount/3,
         umount2/4,
         unlink/3,
         unsetenv/3,
         unshare/3,
         unveil/4,
         version/2,
         waitpid/4,
         write/4]).

-export([alloc/4,
         cap_constant/4,
         cap_enter/3,
         cap_fcntls_get/4,
         cap_fcntls_limit/5,
         cap_getmode/3,
         cap_ioctls_limit/5,
         cap_rights_limit/5,
         chdir/4,
         chmod/5,
         chown/6,
         chroot/4,
         clearenv/3,
         clone/4,
         clone_constant/4,
         close/4,
         connect/5,
         cpid/3,
         environ/3,
         errno_id/4,
         execve/6,
         execvp/5,
         exit/4,
         fcntl/6,
         fcntl_constant/4,
         fexecve/6,
         file_constant/4,
         filter/5,
         fork/3,
         getcwd/3,
         getenv/4,
         getgid/3,
         getgroups/3,
         gethostname/3,
         getopt/4,
         getpgrp/3,
         getpid/3,
         getpriority/5,
         getresgid/3,
         getresuid/3,
         getrlimit/4,
         getsid/4,
         getuid/3,
         ioctl/6,
         ioctl_constant/4,
         iolist_to_bin/4,
         jail/4,
         jail_attach/4,
         jail_remove/4,
         kill/5,
         link/5,
         lseek/6,
         mkdir/5,
         mkfifo/5,
         mount/9,
         mount_constant/4,
         open/6,
         pivot_root/5,
         pledge/5,
         prctl/8,
         prctl_constant/4,
         procctl/7,
         ptrace/7,
         ptrace_constant/4,
         read/5,
         readdir/4,
         rlimit_constant/4,
         rmdir/4,
         seccomp/6,
         seccomp_constant/4,
         select/7,
         setcpid/6,
         setenv/6,
         setgid/4,
         setgroups/4,
         sethostname/4,
         setns/5,
         setopt/5,
         setpgid/5,
         setpriority/6,
         setproctitle/4,
         setresgid/6,
         setresuid/6,
         setrlimit/5,
         setsid/3,
         setuid/4,
         sigaction/5,
         signal_constant/4,
         socket/6,
         symlink/5,
         syscall_constant/4,
         umount/4,
         umount2/5,
         unlink/4,
         unsetenv/4,
         unshare/4,
         unveil/5,
         version/3,
         waitpid/5,
         write/5]).

audit_arch() ->
    Arches = [
        {{"armv6l","linux",4}, audit_arch_arm},
        {{"armv7l","linux",4}, audit_arch_arm},
        {{"i386","linux",4}, audit_arch_i386},
        {{"aarch64","linux",8}, audit_arch_aarch64},
        {{"x86_64","linux",8}, audit_arch_x86_64}
    ],
    [Arch,_,OS|_] = string:tokens(
        erlang:system_info(system_architecture),
        "-"
    ),
    Wordsize = erlang:system_info({wordsize,external}),
    proplists:get_value({Arch,OS,Wordsize}, Arches, enotsup).
wordalign(Offset) ->
    wordalign(Offset, erlang:system_info({wordsize, external})).
wordalign(Offset, Align) ->
    (Align - (Offset rem Align)) rem Align.

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

stdin(Drv, Pids, Data) ->
    case alcove_drv:stdin(Drv, Pids, Data) of
        ok ->
            ok;
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Data])
    end.

stdout(Drv, Pids) ->
    stdout(Drv, Pids, 0).

stdout(Drv, Pids, Timeout) ->
    stdout_1(Drv, Pids, Timeout, []).

stdout_1(Drv, Pids, Timeout, Acc) ->
    case alcove_drv:stdout(Drv, Pids, Timeout) of
        false ->
            lists:reverse(Acc);
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        {alcove_pipe, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply ->
            stdout_1(Drv, Pids, Timeout, [Reply|Acc])
    end.

stderr(Drv, Pids) ->
    stderr(Drv, Pids, 0).

stderr(Drv, Pids, Timeout) ->
    stderr_1(Drv, Pids, Timeout, []).

stderr_1(Drv, Pids, Timeout, Acc) ->
    case alcove_drv:stderr(Drv, Pids, Timeout) of
        false ->
            lists:reverse(Acc);
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        {alcove_pipe, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply ->
            stderr_1(Drv, Pids, Timeout, [Reply|Acc])
    end.

-spec eof(alcove_drv:ref(),[pid_t()]) -> 'ok' | {'error',posix()}.
eof(Drv, Pids) ->
    eof(Drv, Pids, stdin).

-spec eof(alcove_drv:ref(),[pid_t()],'stdin' | 'stdout' | 'stderr')
    -> 'ok' | {'error',posix()}.
eof(_Drv, [], _Stdio) ->
    {error,esrch};
eof(Drv, Pids0, Stdio) ->
    [Pid|Rest] = lists:reverse(Pids0),
    Pids = lists:reverse(Rest),
    Proc = cpid(Drv, Pids),
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

event(Drv, Pids) ->
    event(Drv, Pids, 0).

event(Drv, Pids, Timeout) ->
    alcove_drv:event(Drv, Pids, Timeout).

filter(Calls) when is_list(Calls) ->
    filter({deny, Calls});
filter({allow, Calls}) when is_list(Calls) ->
    [ alcove_proto:call(Call) || Call <-
      alcove_proto:calls() -- Calls ];
filter({deny, Calls}) when is_list(Calls) ->
    [ alcove_proto:call(Call) || Call <-
      sets:to_list(sets:from_list(Calls)) ].

filter(Drv, Pids, Calls) ->
    filter(Drv, Pids, Calls, Calls).

getcpid(Drv, Pids, Pid, Key) ->
    N = indexof(Key, record_info(fields, alcove_pid)),
    case lists:keyfind(Pid, #alcove_pid.pid, alcove:cpid(Drv, Pids)) of
        false ->
            false;
        Child when is_integer(N) ->
            element(N, Child)
    end.

indexof(El, List) ->
    indexof(El, List, 2).
indexof(_El, [], _N) ->
    false;
indexof(El, [El|_], N) ->
    N;
indexof(El, [_|Tail], N) ->
    indexof(El, Tail, N+1).


alloc(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, alloc, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

alloc(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, alloc, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

cap_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

cap_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

cap_enter(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, cap_enter, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

cap_enter(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, cap_enter, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

cap_fcntls_get(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_fcntls_get,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

cap_fcntls_get(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_fcntls_get,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

cap_fcntls_limit(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_fcntls_limit,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

cap_fcntls_limit(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_fcntls_limit,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

cap_getmode(Drv, Pids) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_getmode,
                         [],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

cap_getmode(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_getmode,
                         [],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

cap_ioctls_limit(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_ioctls_limit,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

cap_ioctls_limit(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_ioctls_limit,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

cap_rights_limit(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_rights_limit,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

cap_rights_limit(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         cap_rights_limit,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

chdir(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, chdir, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

chdir(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, chdir, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

chmod(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         chmod,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

chmod(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         chmod,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

chown(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         chown,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

chown(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         chown,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

chroot(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         chroot,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

chroot(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, chroot, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

clearenv(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, clearenv, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

clearenv(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, clearenv, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

clone(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, clone, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

clone(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, clone, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

clone_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         clone_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

clone_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         clone_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

close(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, close, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

close(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, close, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

connect(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         connect,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

connect(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         connect,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

cpid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, cpid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

cpid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, cpid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

environ(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, environ, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

environ(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, environ, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

errno_id(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         errno_id,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

errno_id(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         errno_id,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

execve(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         execve,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

execve(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         execve,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

execvp(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         execvp,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

execvp(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         execvp,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

exit(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, exit, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

exit(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, exit, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

fcntl(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         fcntl,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

fcntl(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         fcntl,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

fcntl_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         fcntl_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

fcntl_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         fcntl_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

fexecve(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         fexecve,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

fexecve(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         fexecve,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

file_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         file_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

file_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         file_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

filter(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         filter,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

filter(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         filter,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

fork(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, fork, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

fork(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, fork, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getcwd(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getcwd, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getcwd(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getcwd, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getenv(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         getenv,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

getenv(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, getenv, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

getgid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getgid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getgid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getgid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getgroups(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getgroups, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getgroups(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getgroups, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

gethostname(Drv, Pids) ->
    case alcove_drv:call(Drv,
                         Pids,
                         gethostname,
                         [],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

gethostname(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         gethostname,
                         [],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getopt(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         getopt,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

getopt(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, getopt, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

getpgrp(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getpgrp, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getpgrp(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getpgrp, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getpid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getpid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getpid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getpid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getpriority(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         getpriority,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

getpriority(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         getpriority,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

getresgid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getresgid, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getresgid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getresgid, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getresuid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getresuid, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getresuid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getresuid, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

getrlimit(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         getrlimit,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

getrlimit(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         getrlimit,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

getsid(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         getsid,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

getsid(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, getsid, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

getuid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getuid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

getuid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getuid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

ioctl(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ioctl,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

ioctl(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ioctl,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

ioctl_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ioctl_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

ioctl_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ioctl_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

iolist_to_bin(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         iolist_to_bin,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

iolist_to_bin(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         iolist_to_bin,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

jail(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, jail, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

jail(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, jail, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

jail_attach(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         jail_attach,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

jail_attach(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         jail_attach,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

jail_remove(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         jail_remove,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

jail_remove(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         jail_remove,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

kill(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         kill,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

kill(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         kill,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

link(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         link,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

link(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         link,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

lseek(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         lseek,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

lseek(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         lseek,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

mkdir(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mkdir,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

mkdir(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mkdir,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

mkfifo(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mkfifo,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

mkfifo(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mkfifo,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

mount(Drv, Pids, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mount,
                         [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6]);
        Reply -> Reply
    end.

mount(Drv, Pids, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6,
      Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mount,
                         [Arg1, Arg2, Arg3, Arg4, Arg5, Arg6],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv,
                          Pids,
                          Arg1,
                          Arg2,
                          Arg3,
                          Arg4,
                          Arg5,
                          Arg6,
                          Timeout]);
        Reply -> Reply
    end.

mount_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mount_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

mount_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         mount_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

open(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         open,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

open(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         open,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

pivot_root(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         pivot_root,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

pivot_root(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         pivot_root,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

pledge(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         pledge,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

pledge(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         pledge,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

prctl(Drv, Pids, Arg1, Arg2, Arg3, Arg4, Arg5) ->
    case alcove_drv:call(Drv,
                         Pids,
                         prctl,
                         [Arg1, Arg2, Arg3, Arg4, Arg5],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4, Arg5]);
        Reply -> Reply
    end.

prctl(Drv, Pids, Arg1, Arg2, Arg3, Arg4, Arg5,
      Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         prctl,
                         [Arg1, Arg2, Arg3, Arg4, Arg5],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4, Arg5, Timeout]);
        Reply -> Reply
    end.

prctl_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         prctl_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

prctl_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         prctl_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

procctl(Drv, Pids, Arg1, Arg2, Arg3, Arg4) ->
    case alcove_drv:call(Drv,
                         Pids,
                         procctl,
                         [Arg1, Arg2, Arg3, Arg4],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4]);
        Reply -> Reply
    end.

procctl(Drv, Pids, Arg1, Arg2, Arg3, Arg4, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         procctl,
                         [Arg1, Arg2, Arg3, Arg4],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4, Timeout]);
        Reply -> Reply
    end.

ptrace(Drv, Pids, Arg1, Arg2, Arg3, Arg4) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ptrace,
                         [Arg1, Arg2, Arg3, Arg4],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4]);
        Reply -> Reply
    end.

ptrace(Drv, Pids, Arg1, Arg2, Arg3, Arg4, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ptrace,
                         [Arg1, Arg2, Arg3, Arg4],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4, Timeout]);
        Reply -> Reply
    end.

ptrace_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ptrace_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

ptrace_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         ptrace_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

read(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         read,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

read(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         read,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

readdir(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         readdir,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

readdir(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         readdir,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

rlimit_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         rlimit_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

rlimit_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         rlimit_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

rmdir(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, rmdir, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

rmdir(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, rmdir, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

seccomp(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         seccomp,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

seccomp(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         seccomp,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

seccomp_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         seccomp_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

seccomp_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         seccomp_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

select(Drv, Pids, Arg1, Arg2, Arg3, Arg4) ->
    case alcove_drv:call(Drv,
                         Pids,
                         select,
                         [Arg1, Arg2, Arg3, Arg4],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4]);
        Reply -> Reply
    end.

select(Drv, Pids, Arg1, Arg2, Arg3, Arg4, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         select,
                         [Arg1, Arg2, Arg3, Arg4],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Arg4, Timeout]);
        Reply -> Reply
    end.

setcpid(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setcpid,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

setcpid(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setcpid,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

setenv(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setenv,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

setenv(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setenv,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

setgid(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setgid,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

setgid(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, setgid, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

setgroups(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setgroups,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

setgroups(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setgroups,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

sethostname(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         sethostname,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

sethostname(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         sethostname,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

setns(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setns,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

setns(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setns,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

setopt(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setopt,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

setopt(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setopt,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

setpgid(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setpgid,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

setpgid(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setpgid,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

setpriority(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setpriority,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

setpriority(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setpriority,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

setproctitle(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setproctitle,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

setproctitle(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setproctitle,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

setresgid(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setresgid,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

setresgid(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setresgid,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

setresuid(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setresuid,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

setresuid(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setresuid,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

setrlimit(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setrlimit,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

setrlimit(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setrlimit,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

setsid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, setsid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

setsid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, setsid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

setuid(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         setuid,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

setuid(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, setuid, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

sigaction(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         sigaction,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

sigaction(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         sigaction,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

signal_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         signal_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

signal_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         signal_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

socket(Drv, Pids, Arg1, Arg2, Arg3) ->
    case alcove_drv:call(Drv,
                         Pids,
                         socket,
                         [Arg1, Arg2, Arg3],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Arg3]);
        Reply -> Reply
    end.

socket(Drv, Pids, Arg1, Arg2, Arg3, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         socket,
                         [Arg1, Arg2, Arg3],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error,
                         [Drv, Pids, Arg1, Arg2, Arg3, Timeout]);
        Reply -> Reply
    end.

symlink(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         symlink,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

symlink(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         symlink,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

syscall_constant(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         syscall_constant,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

syscall_constant(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         syscall_constant,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

umount(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         umount,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

umount(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, umount, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

umount2(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         umount2,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

umount2(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         umount2,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

unlink(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         unlink,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

unlink(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, unlink, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

unsetenv(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         unsetenv,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

unsetenv(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         unsetenv,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

unshare(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv,
                         Pids,
                         unshare,
                         [Arg1],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

unshare(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         unshare,
                         [Arg1],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

unveil(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         unveil,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

unveil(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         unveil,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

version(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, version, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

version(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, version, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

waitpid(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         waitpid,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

waitpid(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         waitpid,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.

write(Drv, Pids, Arg1, Arg2) ->
    case alcove_drv:call(Drv,
                         Pids,
                         write,
                         [Arg1, Arg2],
                         infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2]);
        Reply -> Reply
    end.

write(Drv, Pids, Arg1, Arg2, Timeout) ->
    case alcove_drv:call(Drv,
                         Pids,
                         write,
                         [Arg1, Arg2],
                         Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Arg2, Timeout]);
        Reply -> Reply
    end.
