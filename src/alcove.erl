% GENERATED: DO NOT EDIT
%%% % @noformat

-module(alcove).

-include("alcove.hrl").

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

-type uint8_t() :: 0..16#ff.
-type uint16_t() :: 0..16#ffff.
-type uint32_t() :: 0..16#ffffffff.
-type uint64_t() :: 0..16#ffffffffffffffff.

-type int8_t() :: -16#7f..16#7f.
-type int16_t() :: -16#7fff..16#7fff.
-type int32_t() :: -16#7fffffff..16#7fffffff.
-type int64_t() :: -16#7fffffffffffffff..16#7fffffffffffffff.

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

-type posix() ::
    'e2big'
    | 'eacces'
    | 'eaddrinuse'
    | 'eaddrnotavail'
    | 'eadv'
    | 'eafnosupport'
    | 'eagain'
    | 'ealign'
    | 'ealready'
    | 'ebade'
    | 'ebadf'
    | 'ebadfd'
    | 'ebadmsg'
    | 'ebadr'
    | 'ebadrpc'
    | 'ebadrqc'
    | 'ebadslt'
    | 'ebfont'
    | 'ebusy'
    | 'ecapmode'
    | 'echild'
    | 'echrng'
    | 'ecomm'
    | 'econnaborted'
    | 'econnrefused'
    | 'econnreset'
    | 'edeadlk'
    | 'edeadlock'
    | 'edestaddrreq'
    | 'edirty'
    | 'edom'
    | 'edotdot'
    | 'edquot'
    | 'eduppkg'
    | 'eexist'
    | 'efault'
    | 'efbig'
    | 'ehostdown'
    | 'ehostunreach'
    | 'eidrm'
    | 'einit'
    | 'einprogress'
    | 'eintr'
    | 'einval'
    | 'eio'
    | 'eisconn'
    | 'eisdir'
    | 'eisnam'
    | 'el2hlt'
    | 'el2nsync'
    | 'el3hlt'
    | 'el3rst'
    | 'elbin'
    | 'elibacc'
    | 'elibbad'
    | 'elibexec'
    | 'elibmax'
    | 'elibscn'
    | 'elnrng'
    | 'eloop'
    | 'emfile'
    | 'emlink'
    | 'emsgsize'
    | 'emultihop'
    | 'enametoolong'
    | 'enavail'
    | 'enet'
    | 'enetdown'
    | 'enetreset'
    | 'enetunreach'
    | 'enfile'
    | 'enoano'
    | 'enobufs'
    | 'enocsi'
    | 'enodata'
    | 'enodev'
    | 'enoent'
    | 'enoexec'
    | 'enolck'
    | 'enolink'
    | 'enomem'
    | 'enomsg'
    | 'enonet'
    | 'enopkg'
    | 'enoprotoopt'
    | 'enospc'
    | 'enosr'
    | 'enostr'
    | 'enosym'
    | 'enosys'
    | 'enotblk'
    | 'enotcapable'
    | 'enotconn'
    | 'enotdir'
    | 'enotempty'
    | 'enotnam'
    | 'enotrecoverable'
    | 'enotsock'
    | 'enotsup'
    | 'enotty'
    | 'enotuniq'
    | 'enxio'
    | 'eopnotsupp'
    | 'eoverflow'
    | 'eownerdead'
    | 'eperm'
    | 'epfnosupport'
    | 'epipe'
    | 'eproclim'
    | 'eprocunavail'
    | 'eprogmismatch'
    | 'eprogunavail'
    | 'eproto'
    | 'eprotonosupport'
    | 'eprototype'
    | 'erange'
    | 'erefused'
    | 'eremchg'
    | 'eremdev'
    | 'eremote'
    | 'eremoteio'
    | 'eremoterelease'
    | 'erofs'
    | 'erpcmismatch'
    | 'erremote'
    | 'eshutdown'
    | 'esocktnosupport'
    | 'espipe'
    | 'esrch'
    | 'esrmnt'
    | 'estale'
    | 'esuccess'
    | 'etime'
    | 'etimedout'
    | 'etoomanyrefs'
    | 'etxtbsy'
    | 'euclean'
    | 'eunatch'
    | 'eusers'
    | 'eversion'
    | 'ewouldblock'
    | 'exdev'
    | 'exfull'.

-type alcove_pid_field() ::
    pid
    | flowcontrol
    | signaloneof
    | fdctl
    | stdin
    | stdout
    | stderr.

-type alcove_pid() :: #alcove_pid{}.
-type alcove_rlimit() :: #alcove_rlimit{}.
-type alcove_timeval() :: #alcove_timeval{}.

-type filter() ::
    alcove_proto:calls()
    | []
    | {'deny', alcove_proto:calls() | []}
    | {'allow', alcove_proto:calls() | []}.

-export_type([
    uint8_t/0,
    uint16_t/0,
    uint32_t/0,
    uint64_t/0,
    int8_t/0,
    int16_t/0,
    int32_t/0,
    int64_t/0,

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

-spec alloc(alcove_drv:ref(), [pid_t()], Ptr :: atom()) -> {'ok', binary(), iodata()}.
-spec alloc(alcove_drv:ref(), [pid_t()], Ptr :: cstruct(), timeout()) ->
    {'ok', binary(), iodata()}.

-spec cap_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) -> integer() | 'unknown'.
-spec cap_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    integer() | 'unknown'.

-spec cap_enter(alcove_drv:ref(), [pid_t()]) -> 'ok' | {'error', posix()}.
-spec cap_enter(alcove_drv:ref(), [pid_t()], timeout()) -> 'ok' | {'error', posix()}.

-spec cap_fcntls_get(alcove_drv:ref(), [pid_t()], FD :: fd()) ->
    {'ok', integer()} | {'error', posix()}.
-spec cap_fcntls_get(alcove_drv:ref(), [pid_t()], FD :: fd(), timeout()) ->
    {'ok', integer()} | {'error', posix()}.

-spec cap_fcntls_limit(alcove_drv:ref(), [pid_t()], FD :: fd(), Rights :: constant()) ->
    'ok' | {'error', posix()}.
-spec cap_fcntls_limit(alcove_drv:ref(), [pid_t()], FD :: fd(), Rights :: constant(), timeout()) ->
    'ok' | {'error', posix()}.

-spec cap_getmode(alcove_drv:ref(), [pid_t()]) -> {'ok', 0 | 1} | {'error', posix()}.
-spec cap_getmode(alcove_drv:ref(), [pid_t()], timeout()) -> {'ok', 0 | 1} | {'error', posix()}.

-spec cap_ioctls_limit(alcove_drv:ref(), [pid_t()], FD :: fd(), Rights :: constant()) ->
    'ok' | {'error', posix()}.
-spec cap_ioctls_limit(alcove_drv:ref(), [pid_t()], FD :: fd(), Rights :: constant(), timeout()) ->
    'ok' | {'error', posix()}.

-spec cap_rights_limit(alcove_drv:ref(), [pid_t()], FD :: fd(), Rights :: constant()) ->
    'ok' | {'error', posix()}.
-spec cap_rights_limit(alcove_drv:ref(), [pid_t()], FD :: fd(), Rights :: constant(), timeout()) ->
    'ok' | {'error', posix()}.

-spec chdir(alcove_drv:ref(), [pid_t()], Path :: iodata()) -> 'ok' | {'error', posix()}.
-spec chdir(alcove_drv:ref(), [pid_t()], Path :: iodata(), timeout()) -> 'ok' | {'error', posix()}.

-spec cpid(alcove_drv:ref(), [pid_t()]) -> [alcove_pid()].
-spec cpid(alcove_drv:ref(), [pid_t()], timeout()) -> [alcove_pid()].

-spec chmod(alcove_drv:ref(), [pid_t()], Path :: iodata(), Mode :: mode_t()) ->
    'ok' | {'error', posix()}.
-spec chmod(alcove_drv:ref(), [pid_t()], Path :: iodata(), Mode :: mode_t(), timeout()) ->
    'ok' | {'error', posix()}.

-spec chown(alcove_drv:ref(), [pid_t()], Path :: iodata(), Owner :: uid_t(), Group :: gid_t()) ->
    'ok' | {'error', posix()}.
-spec chown(
    alcove_drv:ref(), [pid_t()], Path :: iodata(), Owner :: uid_t(), Group :: gid_t(), timeout()
) ->
    'ok' | {'error', posix()}.

-spec chroot(alcove_drv:ref(), [pid_t()], Path :: iodata()) -> 'ok' | {'error', posix()}.
-spec chroot(alcove_drv:ref(), [pid_t()], Path :: iodata(), timeout()) -> 'ok' | {'error', posix()}.

-spec clearenv(alcove_drv:ref(), [pid_t()]) -> 'ok' | {'error', posix()}.
-spec clearenv(alcove_drv:ref(), [pid_t()], timeout()) -> 'ok' | {'error', posix()}.

-spec clone(alcove_drv:ref(), [pid_t()], Flags :: int32_t() | [constant()]) ->
    {'ok', pid_t()} | {'error', posix()}.
-spec clone(alcove_drv:ref(), [pid_t()], Flags :: int32_t() | [constant()], timeout()) ->
    {'ok', pid_t()} | {'error', posix()}.

-spec clone_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) -> 'unknown' | int32_t().
-spec clone_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | int32_t().

-spec close(alcove_drv:ref(), [pid_t()], FD :: fd()) -> 'ok' | {'error', posix()}.
-spec close(alcove_drv:ref(), [pid_t()], FD :: fd(), timeout()) -> 'ok' | {'error', posix()}.

-spec connect(alcove_drv:ref(), [pid_t()], FD :: fd(), Sockaddr :: [] | cstruct()) ->
    'ok' | {'error', posix()}.
-spec connect(alcove_drv:ref(), [pid_t()], FD :: fd(), Sockaddr :: [] | cstruct(), timeout()) ->
    'ok' | {'error', posix()}.

-spec environ(alcove_drv:ref(), [pid_t()]) -> [binary()].
-spec environ(alcove_drv:ref(), [pid_t()], timeout()) -> [binary()].

-spec errno_id(alcove_drv:ref(), [pid_t()], Errno :: int32_t()) -> posix().
-spec errno_id(alcove_drv:ref(), [pid_t()], Errno :: int32_t(), timeout()) -> posix().

-spec execve(alcove_drv:ref(), [pid_t()], Arg0 :: iodata(), Argv :: [iodata()], Env :: [iodata()]) ->
    'ok' | {'error', posix()}.
-spec execve(
    alcove_drv:ref(), [pid_t()], Arg0 :: iodata(), Argv :: [iodata()], Env :: [iodata()], timeout()
) ->
    'ok' | {'error', posix()}.

-spec execvp(alcove_drv:ref(), [pid_t()], Arg0 :: iodata(), Argv :: [iodata()]) ->
    'ok' | {'error', posix()}.
-spec execvp(alcove_drv:ref(), [pid_t()], Arg0 :: iodata(), Argv :: [iodata()], timeout()) ->
    'ok' | {'error', posix()}.

-spec exit(alcove_drv:ref(), [pid_t()], Status :: int32_t()) -> 'ok'.
-spec exit(alcove_drv:ref(), [pid_t()], Status :: int32_t(), timeout()) -> 'ok'.

-spec fcntl(alcove_drv:ref(), [pid_t()], FD :: fd(), Cmd :: constant(), Arg :: int64_t()) ->
    {'ok', int64_t()} | {'error', posix()}.
-spec fcntl(
    alcove_drv:ref(), [pid_t()], FD :: fd(), Cmd :: constant(), Arg :: int64_t(), timeout()
) ->
    {'ok', int64_t()} | {'error', posix()}.

-spec fcntl_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) -> integer() | 'unknown'.
-spec fcntl_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    integer() | 'unknown'.

-spec fexecve(alcove_drv:ref(), [pid_t()], FD :: fd(), Argv :: [iodata()], Env :: [iodata()]) ->
    'ok' | {'error', posix()}.
-spec fexecve(
    alcove_drv:ref(), [pid_t()], FD :: fd(), Argv :: [iodata()], Env :: [iodata()], timeout()
) ->
    'ok' | {'error', posix()}.

-spec file_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) -> non_neg_integer() | 'unknown'.
-spec file_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    non_neg_integer() | 'unknown'.

-spec filter(
    alcove_drv:ref(), [pid_t()], Calls :: [alcove_proto:call()], Calls :: [alcove_proto:call()]
) ->
    ok | {'error', 'einval'}.
-spec filter(
    alcove_drv:ref(),
    [pid_t()],
    Calls :: [alcove_proto:call()],
    Calls :: [alcove_proto:call()],
    timeout()
) ->
    ok | {'error', 'einval'}.

-spec fork(alcove_drv:ref(), [pid_t()]) -> {'ok', pid_t()} | {'error', posix()}.
-spec fork(alcove_drv:ref(), [pid_t()], timeout()) -> {'ok', pid_t()} | {'error', posix()}.

-spec getcwd(alcove_drv:ref(), [pid_t()]) -> {'ok', binary()} | {'error', posix()}.
-spec getcwd(alcove_drv:ref(), [pid_t()], timeout()) -> {'ok', binary()} | {'error', posix()}.

-spec getenv(alcove_drv:ref(), [pid_t()], Name :: iodata()) -> binary() | 'false'.
-spec getenv(alcove_drv:ref(), [pid_t()], Name :: iodata(), timeout()) -> binary() | 'false'.

-spec getgid(alcove_drv:ref(), [pid_t()]) -> gid_t().
-spec getgid(alcove_drv:ref(), [pid_t()], timeout()) -> gid_t().

-spec getgroups(alcove_drv:ref(), [pid_t()]) -> {ok, [gid_t()]} | {error, posix()}.
-spec getgroups(alcove_drv:ref(), [pid_t()], timeout()) -> {ok, [gid_t()]} | {error, posix()}.

-spec gethostname(alcove_drv:ref(), [pid_t()]) -> {'ok', binary()} | {'error', posix()}.
-spec gethostname(alcove_drv:ref(), [pid_t()], timeout()) -> {'ok', binary()} | {'error', posix()}.

-spec getopt(alcove_drv:ref(), [pid_t()], Opt :: atom()) -> 'false' | int32_t().
-spec getopt(alcove_drv:ref(), [pid_t()], Opt :: atom(), timeout()) -> 'false' | int32_t().

-spec getpgrp(alcove_drv:ref(), [pid_t()]) -> pid_t().
-spec getpgrp(alcove_drv:ref(), [pid_t()], timeout()) -> pid_t().

-spec getpid(alcove_drv:ref(), [pid_t()]) -> pid_t().
-spec getpid(alcove_drv:ref(), [pid_t()], timeout()) -> pid_t().

-spec getpriority(alcove_drv:ref(), [pid_t()], Which :: constant(), Who :: int32_t()) ->
    {'ok', int32_t()} | {'error', posix()}.
-spec getpriority(alcove_drv:ref(), [pid_t()], Which :: constant(), Who :: int32_t(), timeout()) ->
    {'ok', int32_t()} | {'error', posix()}.

-spec getresgid(alcove_drv:ref(), [pid_t()]) ->
    {'ok', Real :: gid_t(), Effective :: gid_t(), Saved :: gid_t()} | {'error', posix()}.
-spec getresgid(alcove_drv:ref(), [pid_t()], timeout()) ->
    {'ok', Real :: gid_t(), Effective :: gid_t(), Saved :: gid_t()} | {'error', posix()}.

-spec getresuid(alcove_drv:ref(), [pid_t()]) ->
    {'ok', Real :: uid_t(), Effective :: uid_t(), Saved :: uid_t()} | {'error', posix()}.
-spec getresuid(alcove_drv:ref(), [pid_t()], timeout()) ->
    {'ok', Real :: uid_t(), Effective :: uid_t(), Saved :: uid_t()} | {'error', posix()}.

-spec getrlimit(alcove_drv:ref(), [pid_t()], constant()) ->
    {'ok', alcove_rlimit()} | {'error', posix()}.
-spec getrlimit(alcove_drv:ref(), [pid_t()], constant(), timeout()) ->
    {'ok', alcove_rlimit()} | {'error', posix()}.

-spec getsid(alcove_drv:ref(), [pid_t()], OSPid :: pid_t()) -> {'ok', pid_t()} | {'error', posix()}.
-spec getsid(alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), timeout()) ->
    {'ok', pid_t()} | {'error', posix()}.

-spec getuid(alcove_drv:ref(), [pid_t()]) -> uid_t().
-spec getuid(alcove_drv:ref(), [pid_t()], timeout()) -> uid_t().

-spec ioctl(alcove_drv:ref(), [pid_t()], FD :: fd(), Request :: constant(), Argp :: cstruct()) ->
    {'ok', integer(), iodata()} | {'error', posix()}.
-spec ioctl(
    alcove_drv:ref(), [pid_t()], FD :: fd(), Request :: constant(), Argp :: cstruct(), timeout()
) ->
    {'ok', integer(), iodata()} | {'error', posix()}.

-spec ioctl_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) -> integer() | 'unknown'.
-spec ioctl_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    integer() | 'unknown'.

-spec iolist_to_bin(alcove_drv:ref(), [pid_t()], iodata()) -> binary().
-spec iolist_to_bin(alcove_drv:ref(), [pid_t()], iodata(), timeout()) -> binary().

-spec jail(alcove_drv:ref(), [pid_t()], cstruct()) -> {'ok', int32_t()} | {'error', posix()}.
-spec jail(alcove_drv:ref(), [pid_t()], cstruct(), timeout()) ->
    {'ok', int32_t()} | {'error', posix()}.

-spec jail_attach(alcove_drv:ref(), [pid_t()], int32_t()) -> 'ok' | {'error', posix()}.
-spec jail_attach(alcove_drv:ref(), [pid_t()], int32_t(), timeout()) -> 'ok' | {'error', posix()}.

-spec jail_remove(alcove_drv:ref(), [pid_t()], int32_t()) -> 'ok' | {'error', posix()}.
-spec jail_remove(alcove_drv:ref(), [pid_t()], int32_t(), timeout()) -> 'ok' | {'error', posix()}.

-spec kill(alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), Signal :: constant()) ->
    'ok' | {'error', posix()}.
-spec kill(alcove_drv:ref(), [pid_t()], OSPID :: pid_t(), Signal :: constant(), timeout()) ->
    'ok' | {'error', posix()}.

-spec link(alcove_drv:ref(), [pid_t()], OldPath :: iodata(), NewPath :: iodata()) ->
    'ok' | {error, posix()}.
-spec link(alcove_drv:ref(), [pid_t()], OldPath :: iodata(), NewPath :: iodata(), timeout()) ->
    'ok' | {error, posix()}.

-spec lseek(alcove_drv:ref(), [pid_t()], FD :: fd(), Offset :: off_t(), Whence :: int32_t()) ->
    'ok' | {'error', posix()}.
-spec lseek(
    alcove_drv:ref(), [pid_t()], FD :: fd(), Offset :: off_t(), Whence :: int32_t(), timeout()
) ->
    'ok' | {'error', posix()}.

-spec mkdir(alcove_drv:ref(), [pid_t()], Path :: iodata(), Mode :: mode_t()) ->
    'ok' | {'error', posix()}.
-spec mkdir(alcove_drv:ref(), [pid_t()], Path :: iodata(), Mode :: mode_t(), timeout()) ->
    'ok' | {'error', posix()}.

-spec mkfifo(alcove_drv:ref(), [pid_t()], Path :: iodata(), Mode :: mode_t()) ->
    'ok' | {'error', posix()}.
-spec mkfifo(alcove_drv:ref(), [pid_t()], Path :: iodata(), Mode :: mode_t(), timeout()) ->
    'ok' | {'error', posix()}.

-spec mount(
    alcove_drv:ref(),
    [pid_t()],
    Source :: iodata(),
    Target :: iodata(),
    FSType :: iodata(),
    Flags :: uint64_t() | [constant()],
    Data :: iodata(),
    Options :: iodata()
) -> 'ok' | {'error', posix()}.
-spec mount(
    alcove_drv:ref(),
    [pid_t()],
    Source :: iodata(),
    Target :: iodata(),
    FSType :: iodata(),
    Flags :: uint64_t() | [constant()],
    Data :: iodata(),
    Options :: iodata(),
    timeout()
) -> 'ok' | {'error', posix()}.

-spec mount_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) -> 'unknown' | uint64_t().
-spec mount_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | uint64_t().

-spec open(
    alcove_drv:ref(),
    [pid_t()],
    Path :: iodata(),
    Flags :: int32_t() | [constant()],
    Mode :: mode_t()
) ->
    {'ok', fd()} | {'error', posix()}.
-spec open(
    alcove_drv:ref(),
    [pid_t()],
    Path :: iodata(),
    Flags :: int32_t() | [constant()],
    Mode :: mode_t(),
    timeout()
) ->
    {'ok', fd()} | {'error', posix()}.

-spec pledge(alcove_drv:ref(), [pid_t()], Promises :: iodata(), ExecPromises :: iodata()) ->
    'ok' | {'error', posix()}.
-spec pledge(
    alcove_drv:ref(), [pid_t()], Promises :: iodata(), ExecPromises :: iodata(), timeout()
) ->
    'ok' | {'error', posix()}.

-spec pivot_root(alcove_drv:ref(), [pid_t()], NewRoot :: iodata(), PutOld :: iodata()) ->
    'ok' | {'error', posix()}.
-spec pivot_root(alcove_drv:ref(), [pid_t()], NewRoot :: iodata(), PutOld :: iodata(), timeout()) ->
    'ok' | {'error', posix()}.

-type ptr_arg() :: binary() | constant() | cstruct().
-type ptr_val() :: binary() | integer() | cstruct().
-spec prctl(alcove_drv:ref(), [pid_t()], constant(), ptr_arg(), ptr_arg(), ptr_arg(), ptr_arg()) ->
    {'ok', integer(), ptr_val(), ptr_val(), ptr_val(), ptr_val()} | {'error', posix()}.
-spec prctl(
    alcove_drv:ref(), [pid_t()], constant(), ptr_arg(), ptr_arg(), ptr_arg(), ptr_arg(), timeout()
) ->
    {'ok', integer(), ptr_val(), ptr_val(), ptr_val(), ptr_val()} | {'error', posix()}.

-spec prctl_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) ->
    'unknown' | non_neg_integer().
-spec prctl_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | non_neg_integer().

-spec procctl(
    alcove_drv:ref(),
    [pid_t()],
    IDType :: constant(),
    ID :: pid_t(),
    Cmd :: constant(),
    Data :: [] | cstruct()
) ->
    {'ok', binary(), cstruct()} | {'error', posix()}.
-spec procctl(
    alcove_drv:ref(),
    [pid_t()],
    IDType :: constant(),
    ID :: pid_t(),
    Cmd :: constant(),
    Data :: [] | cstruct(),
    timeout()
) ->
    {'ok', binary(), cstruct()} | {'error', posix()}.

-spec ptrace(
    alcove_drv:ref(),
    [pid_t()],
    Request :: constant(),
    OSPid :: pid_t(),
    Addr :: ptr_arg(),
    Data :: ptr_arg()
) ->
    {'ok', integer(), ptr_val(), ptr_val()} | {'error', posix()}.
-spec ptrace(
    alcove_drv:ref(),
    [pid_t()],
    Request :: constant(),
    OSPid :: pid_t(),
    Addr :: ptr_arg(),
    Data :: ptr_arg(),
    timeout()
) ->
    {'ok', integer(), ptr_val(), ptr_val()} | {'error', posix()}.

-spec ptrace_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) ->
    'unknown' | integer().
-spec ptrace_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | integer().

-spec read(alcove_drv:ref(), [pid_t()], FD :: fd(), Count :: size_t()) ->
    {'ok', binary()} | {'error', posix()}.
-spec read(alcove_drv:ref(), [pid_t()], FD :: fd(), Count :: size_t(), timeout()) ->
    {'ok', binary()} | {'error', posix()}.

-spec readdir(alcove_drv:ref(), [pid_t()], Path :: iodata()) ->
    {'ok', [binary()]} | {'error', posix()}.
-spec readdir(alcove_drv:ref(), [pid_t()], Path :: iodata(), timeout()) ->
    {'ok', [binary()]} | {'error', posix()}.

-spec rlimit_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) ->
    'unknown' | non_neg_integer().
-spec rlimit_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | non_neg_integer().

-spec rmdir(alcove_drv:ref(), [pid_t()], Path :: iodata()) -> 'ok' | {'error', posix()}.
-spec rmdir(alcove_drv:ref(), [pid_t()], Path :: iodata(), timeout()) -> 'ok' | {'error', posix()}.

-spec seccomp(
    alcove_drv:ref(), [pid_t()], Operation :: constant(), Flags :: constant(), Prog :: cstruct()
) ->
    'ok' | {'error', posix()}.
-spec seccomp(
    alcove_drv:ref(),
    [pid_t()],
    Operation :: constant(),
    Flags :: constant(),
    Prog :: cstruct(),
    timeout()
) ->
    'ok' | {'error', posix()}.

-spec seccomp_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) ->
    'unknown' | non_neg_integer().
-spec seccomp_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | non_neg_integer().

-spec select(
    alcove_drv:ref(),
    [pid_t()],
    Readfds :: fd_set(),
    Writefds :: fd_set(),
    Exceptfds :: fd_set(),
    Timeval :: [] | 'null' | alcove_timeval()
) -> {ok, Readset :: fd_set(), Writeset :: fd_set(), Exceptset :: fd_set()} | {'error', posix()}.
-spec select(
    alcove_drv:ref(),
    [pid_t()],
    Readfds :: fd_set(),
    Writefds :: fd_set(),
    Exceptfds :: fd_set(),
    Timeval :: [] | 'null' | alcove_timeval(),
    timeout()
) -> {ok, Readset :: fd_set(), Writeset :: fd_set(), Exceptset :: fd_set()} | {'error', posix()}.

-spec setcpid(
    alcove_drv:ref(), [pid_t()], Child :: pid_t(), Opt :: alcove_pid_field(), Val :: int32_t()
) -> boolean().
-spec setcpid(
    alcove_drv:ref(),
    [pid_t()],
    Child :: pid_t(),
    Opt :: alcove_pid_field(),
    Val :: int32_t(),
    timeout()
) ->
    boolean().

-spec setenv(
    alcove_drv:ref(), [pid_t()], Name :: iodata(), Value :: iodata(), Overwrite :: int32_t()
) ->
    'ok' | {'error', posix()}.
-spec setenv(
    alcove_drv:ref(),
    [pid_t()],
    Name :: iodata(),
    Value :: iodata(),
    Overwrite :: int32_t(),
    timeout()
) ->
    'ok' | {'error', posix()}.

-spec setgid(alcove_drv:ref(), [pid_t()], Gid :: gid_t()) -> 'ok' | {'error', posix()}.
-spec setgid(alcove_drv:ref(), [pid_t()], Gid :: gid_t(), timeout()) -> 'ok' | {'error', posix()}.

-spec setgroups(alcove_drv:ref(), [pid_t()], Groups :: [gid_t()]) -> 'ok' | {'error', posix()}.
-spec setgroups(alcove_drv:ref(), [pid_t()], Groups :: [gid_t()], timeout()) ->
    'ok' | {'error', posix()}.

-spec sethostname(alcove_drv:ref(), [pid_t()], Hostname :: iodata()) -> 'ok' | {'error', posix()}.
-spec sethostname(alcove_drv:ref(), [pid_t()], Hostname :: iodata(), timeout()) ->
    'ok' | {'error', posix()}.

-spec setns(alcove_drv:ref(), [pid_t()], FD :: fd(), NSType :: constant()) ->
    'ok' | {'error', posix()}.
-spec setns(alcove_drv:ref(), [pid_t()], FD :: fd(), NSType :: constant(), timeout()) ->
    'ok' | {'error', posix()}.

-spec setopt(alcove_drv:ref(), [pid_t()], Opt :: atom(), Val :: int32_t()) -> boolean().
-spec setopt(alcove_drv:ref(), [pid_t()], Opt :: atom(), Val :: int32_t(), timeout()) -> boolean().

-spec setpgid(alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), Pgid :: pid_t()) ->
    'ok' | {'error', posix()}.
-spec setpgid(alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), Pgid :: pid_t(), timeout()) ->
    'ok' | {'error', posix()}.

-spec setpriority(
    alcove_drv:ref(), [pid_t()], Which :: constant(), Who :: int32_t(), Prio :: int32_t()
) ->
    'ok' | {'error', posix()}.
-spec setpriority(
    alcove_drv:ref(), [pid_t()], Which :: constant(), Who :: int32_t(), Prio :: int32_t(), timeout()
) ->
    'ok' | {'error', posix()}.

-spec setproctitle(alcove_drv:ref(), [pid_t()], Title :: iodata()) -> 'ok'.
-spec setproctitle(alcove_drv:ref(), [pid_t()], Title :: iodata(), timeout()) -> 'ok'.

-spec setresgid(
    alcove_drv:ref(), [pid_t()], Real :: gid_t(), Effective :: gid_t(), Saved :: gid_t()
) ->
    'ok' | {'error', posix()}.
-spec setresgid(
    alcove_drv:ref(), [pid_t()], Real :: gid_t(), Effective :: gid_t(), Saved :: gid_t(), timeout()
) ->
    'ok' | {'error', posix()}.

-spec setresuid(
    alcove_drv:ref(), [pid_t()], Real :: uid_t(), Effective :: uid_t(), Saved :: uid_t()
) ->
    'ok' | {'error', posix()}.
-spec setresuid(
    alcove_drv:ref(), [pid_t()], Real :: uid_t(), Effective :: uid_t(), Saved :: uid_t(), timeout()
) ->
    'ok' | {'error', posix()}.

-spec setrlimit(alcove_drv:ref(), [pid_t()], Resource :: constant(), Limit :: alcove_rlimit()) ->
    'ok' | {'error', posix()}.
-spec setrlimit(
    alcove_drv:ref(), [pid_t()], Resource :: constant(), Limit :: alcove_rlimit(), timeout()
) ->
    'ok' | {'error', posix()}.

-spec setsid(alcove_drv:ref(), [pid_t()]) -> {ok, OSPid :: pid_t()} | {error, posix()}.
-spec setsid(alcove_drv:ref(), [pid_t()], timeout()) -> {ok, OSPid :: pid_t()} | {error, posix()}.

-spec setuid(alcove_drv:ref(), [pid_t()], User :: uid_t()) -> 'ok' | {'error', posix()}.
-spec setuid(alcove_drv:ref(), [pid_t()], User :: uid_t(), timeout()) -> 'ok' | {'error', posix()}.

-spec sigaction(alcove_drv:ref(), [pid_t()], Signum :: constant(), Handler :: [] | atom()) ->
    {'ok', OldHandler :: atom()} | {'error', posix()}.
-spec sigaction(
    alcove_drv:ref(), [pid_t()], Signum :: constant(), Handler :: [] | atom(), timeout()
) ->
    {'ok', OldHandler :: atom()} | {'error', posix()}.

-spec signal_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) ->
    'unknown' | non_neg_integer().
-spec signal_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | non_neg_integer().

-spec socket(
    alcove_drv:ref(), [pid_t()], Domain :: constant(), Type :: constant(), Protocol :: int32_t()
) ->
    {'ok', fd()} | {'error', posix()}.
-spec socket(
    alcove_drv:ref(),
    [pid_t()],
    Domain :: constant(),
    Type :: constant(),
    Protocol :: int32_t(),
    timeout()
) ->
    {'ok', fd()} | {'error', posix()}.

-spec syscall_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom()) ->
    'unknown' | non_neg_integer().
-spec syscall_constant(alcove_drv:ref(), [pid_t()], Symbol :: atom(), timeout()) ->
    'unknown' | non_neg_integer().

-spec symlink(alcove_drv:ref(), [pid_t()], OldPath :: iodata(), NewPath :: iodata()) ->
    'ok' | {error, posix()}.
-spec symlink(alcove_drv:ref(), [pid_t()], OldPath :: iodata(), NewPath :: iodata(), timeout()) ->
    'ok' | {error, posix()}.

-spec unlink(alcove_drv:ref(), [pid_t()], Path :: iodata()) -> 'ok' | {error, posix()}.
-spec unlink(alcove_drv:ref(), [pid_t()], Path :: iodata(), timeout()) -> 'ok' | {error, posix()}.

-spec umount(alcove_drv:ref(), [pid_t()], Path :: iodata()) -> 'ok' | {error, posix()}.
-spec umount(alcove_drv:ref(), [pid_t()], Path :: iodata(), timeout()) -> 'ok' | {error, posix()}.

-spec umount2(alcove_drv:ref(), [pid_t()], Path :: iodata(), Flags :: int32_t() | [constant()]) ->
    'ok' | {error, posix()}.
-spec umount2(
    alcove_drv:ref(), [pid_t()], Path :: iodata(), Flags :: int32_t() | [constant()], timeout()
) ->
    'ok' | {error, posix()}.

-spec unsetenv(alcove_drv:ref(), [pid_t()], Name :: iodata()) -> 'ok' | {error, posix()}.
-spec unsetenv(alcove_drv:ref(), [pid_t()], Name :: iodata(), timeout()) -> 'ok' | {error, posix()}.

-spec unshare(alcove_drv:ref(), [pid_t()], Flags :: int32_t() | [constant()]) ->
    'ok' | {'error', posix()}.
-spec unshare(alcove_drv:ref(), [pid_t()], Flags :: int32_t() | [constant()], timeout()) ->
    'ok' | {'error', posix()}.

-spec unveil(alcove_drv:ref(), [pid_t()], Path :: iodata(), Permissions :: iodata()) ->
    'ok' | {'error', posix()}.
-spec unveil(alcove_drv:ref(), [pid_t()], Path :: iodata(), Permissions :: iodata(), timeout()) ->
    'ok' | {'error', posix()}.

-type waitpid_value() ::
    {exit_status, int32_t()}
    | {termsig, atom()}
    | {stopsig, atom()}
    | continued.
-spec waitpid(alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), Options :: int32_t() | [constant()]) ->
    {'ok', pid_t(), WaitStatus :: [waitpid_value()]} | {'error', posix()}.
-spec waitpid(
    alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), Options :: int32_t() | [constant()], timeout()
) ->
    {'ok', pid_t(), WaitStatus :: [waitpid_value()]} | {'error', posix()}.

-spec write(alcove_drv:ref(), [pid_t()], FD :: fd(), Buf :: iodata()) ->
    {'ok', Count :: ssize_t()} | {'error', posix()}.
-spec write(alcove_drv:ref(), [pid_t()], FD :: fd(), Buf :: iodata(), timeout()) ->
    {'ok', Count :: ssize_t()} | {'error', posix()}.

-spec version(alcove_drv:ref(), [pid_t()]) -> binary().
-spec version(alcove_drv:ref(), [pid_t()], timeout()) -> binary().

% Static functions

-export([
    audit_arch/0,
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
    getcpid/4
]).

% @doc Get seccomp system architecture
-spec audit_arch() -> atom().
audit_arch() ->
    Arches = [
        {{"armv6l", "linux", 4}, audit_arch_arm},
        {{"armv7l", "linux", 4}, audit_arch_arm},
        {{"i386", "linux", 4}, audit_arch_i386},
        {{"aarch64", "linux", 8}, audit_arch_aarch64},
        {{"x86_64", "linux", 8}, audit_arch_x86_64}
    ],
    [Arch, _, OS | _] = string:tokens(
        erlang:system_info(system_architecture),
        "-"
    ),
    Wordsize = erlang:system_info({wordsize, external}),
    proplists:get_value({Arch, OS, Wordsize}, Arches, enotsup).

wordalign(Offset) ->
    wordalign(Offset, erlang:system_info({wordsize, external})).
wordalign(Offset, Align) ->
    (Align - (Offset rem Align)) rem Align.

% @doc Convert constant to integer
-spec define(alcove_drv:ref(), [pid_t()], atom() | [atom()]) -> integer().
define(Drv, ForkChain, Constant) when is_atom(Constant) ->
    define(Drv, ForkChain, [Constant]);
define(Drv, ForkChain, Constants) when is_list(Constants) ->
    lists:foldl(
        fun
            (Constant, Result) when is_atom(Constant) ->
                Val = define_constant(Drv, ForkChain, Constant),
                Result bxor Val;
            (Val, Result) when is_integer(Val) ->
                Result bxor Val
        end,
        0,
        Constants
    ).

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
define_foreach(Drv, ForkChain, Constant, [Fun | Rest]) ->
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

% @doc Send data to stdin of the process
-spec stdin(alcove_drv:ref(), [pid_t()], iodata()) -> 'ok'.
stdin(Drv, Pids, Data) ->
    case alcove_drv:stdin(Drv, Pids, Data) of
        ok ->
            ok;
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Data])
    end.

% @doc Read stdout from the process
-spec stdout(alcove_drv:ref(), [pid_t()]) -> [binary()].
stdout(Drv, Pids) ->
    stdout(Drv, Pids, 0).

-spec stdout(alcove_drv:ref(), [pid_t()], timeout()) -> [binary()].
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
            stdout_1(Drv, Pids, Timeout, [Reply | Acc])
    end.

% @doc Read stderr from the process
-spec stderr(alcove_drv:ref(), [pid_t()]) -> [binary()].
stderr(Drv, Pids) ->
    stderr(Drv, Pids, 0).

-spec stderr(alcove_drv:ref(), [pid_t()], timeout()) -> [binary()].
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
            stderr_1(Drv, Pids, Timeout, [Reply | Acc])
    end.

% @doc Close stdin of the process
-spec eof(alcove_drv:ref(), [pid_t()]) -> 'ok' | {'error', posix()}.
eof(Drv, Pids) ->
    eof(Drv, Pids, stdin).

% @doc Close stdin, stdout or stderr of the process
-spec eof(alcove_drv:ref(), [pid_t()], 'stdin' | 'stdout' | 'stderr') ->
    'ok' | {'error', posix()}.
eof(_Drv, [], _Stdio) ->
    {error, esrch};
eof(Drv, Pids0, Stdio) ->
    [Pid | Rest] = lists:reverse(Pids0),
    Pids = lists:reverse(Rest),
    Proc = cpid(Drv, Pids),
    case lists:keyfind(Pid, 2, Proc) of
        false ->
            {error, esrch};
        N ->
            eof_1(Drv, Pids, N, Stdio)
    end.

eof_1(Drv, Pids, #alcove_pid{stdin = FD}, stdin) ->
    close(Drv, Pids, FD);
eof_1(Drv, Pids, #alcove_pid{stdout = FD}, stdout) ->
    close(Drv, Pids, FD);
eof_1(Drv, Pids, #alcove_pid{stderr = FD}, stderr) ->
    close(Drv, Pids, FD).

% @doc Get events generated by alcove port process
%
% event/1,2 is used to retrieve async messages returned from the
% port, such as caught signals, the exit status or the termination
% signal.
-spec event(alcove_drv:ref(), [pid_t()]) -> term().
event(Drv, Pids) ->
    event(Drv, Pids, 0).

% @doc Get events generated by alcove port process
-spec event(alcove_drv:ref(), [pid_t()], timeout()) -> term().
event(Drv, Pids, Timeout) ->
    alcove_drv:event(Drv, Pids, Timeout).

% @doc Create a call filter list
%
% Generate a list of calls to filter for filter/3,4. By default, calls
% are blocked. To restrict the alcove control process to a subset of
% calls, use the allow tuple:
%
% ```
% # alcove process restricted to fork, clone, getpid
% alcove:filter({allow, [fork, clone, getpid]})
% '''
-spec filter(filter()) -> [uint8_t()].
filter(Calls) when is_list(Calls) ->
    filter({deny, Calls});
filter({allow, Calls}) when is_list(Calls) ->
    [
        alcove_proto:call(Call)
     || Call <-
            alcove_proto:calls() -- Calls
    ];
filter({deny, Calls}) when is_list(Calls) ->
    [
        alcove_proto:call(Call)
     || Call <-
            sets:to_list(sets:from_list(Calls))
    ].

% @doc Restrict calls available to an alcove control process
%
% filter/3 restrict calls available to an alcove control process. Restricted
% control processes continue to proxy data and monitor and reap
% subprocesses.
%
% Invoking a filtered call will crash the process with 'undef'.
%
% If the filter call is filtered, subsequent calls to filter/3,4 will fail.
%
% Once added, the call cannot be removed from the filter set. Passing an
% empty list ([]) specifies the current filter set should not be modified.
%
% Filters are inherited by the child process from the parent. filter/3
% specifies the subprocess should use the same filter as the parent:
%
% ```
% {ok, Ctrl} = alcove_drv:start(),
% {ok, Task} = alcove:fork(Ctrl, []),
%
% Calls = alcove:filter([fork]),
% % equivalent to: alcove:filter(Ctrl, [], Calls, Calls)
% ok = alcove:filter(Ctrl, [], Calls),
% {'EXIT', {undef, _}} = (catch alcove:fork(Ctrl, [])).
% '''
-spec filter(alcove_drv:ref(), [pid_t()], [alcove_proto:call()]) -> ok | {'error', 'einval'}.
filter(Drv, Pids, Calls) ->
    filter(Drv, Pids, Calls, Calls).

% @doc Get control process attributes
%
% Retrieves attributes set by the alcove control process for a
% child process.
%
% * flowcontrol
%
%   Number of messages allowed from process:
%
%         -1 : flowcontrol disabled
%
%         0 : stdout/stderr for process is not read
%
%         1+ : read this many messages from the process
%
% * signaloneof
%
%   Signal sent to child process on shutdown.
-spec getcpid(alcove_drv:ref(), [pid_t()], pid_t(), alcove_pid_field()) -> 'false' | int32_t().
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
indexof(El, [El | _], N) ->
    N;
indexof(El, [_ | Tail], N) ->
    indexof(El, Tail, N + 1).


% @doc Allocate memory
% 
% Test memory allocation.

alloc(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, alloc, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

% @doc Allocate memory
% 
% Test memory allocation.

alloc(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, alloc, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc Convert capsicum constants to integer
% 
% FreeBSD only.

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

% @doc Convert capsicum constants to integer
% 
% FreeBSD only.

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

% @doc cap_enter(2): place process into capability mode
% 
% FreeBSD only.

cap_enter(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, cap_enter, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc cap_enter(2): place process into capability mode
% 
% FreeBSD only.

cap_enter(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, cap_enter, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc cap_fcntls_get(2): get allowed fcntl commands in capability mode
% 
% FreeBSD only

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

% @doc cap_fcntls_get(2): get allowed fcntl commands in capability mode
% 
% FreeBSD only

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

% @doc cap_fcntls_limit(2): manage fcntl commands in capability mode
% 
% FreeBSD only.

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

% @doc cap_fcntls_limit(2): manage fcntl commands in capability mode
% 
% FreeBSD only.

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

% @doc cap_getmode(2): check if capability mode is enabled
% 
% FreeBSD only.

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

% @doc cap_getmode(2): check if capability mode is enabled
% 
% FreeBSD only.

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

% @doc cap_ioctls_limit(2): manage allowed ioctl commands
% 
% FreeBSD only.

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

% @doc cap_ioctls_limit(2): manage allowed ioctl commands
% 
% FreeBSD only.

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

% @doc cap_rights_limit(2): manage process capabilities
% 
% FreeBSD only.

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

% @doc cap_rights_limit(2): manage process capabilities
% 
% FreeBSD only.

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

% @doc chdir(2): change process current working directory

chdir(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, chdir, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

% @doc chdir(2): change process current working directory

chdir(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, chdir, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc chmod(2): change file permissions

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

% @doc chmod(2): change file permissions

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

% @doc chown(2): change file ownership

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

% @doc chown(2): change file ownership

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

% @doc chroot(2): change root directory

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

% @doc chroot(2): change root directory

chroot(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, chroot, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc clearenv(3): zero process environment

clearenv(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, clearenv, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc clearenv(3): zero process environment

clearenv(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, clearenv, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc clone(2): create a new process
% 
% Linux only.

clone(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, clone, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

% @doc clone(2): create a new process
% 
% Linux only.

clone(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, clone, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% Map clone(2) symbols to integer constants
% 
% Linux only.

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

% Map clone(2) symbols to integer constants
% 
% Linux only.

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

% @doc close(2): close a file descriptor

close(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, close, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

% @doc close(2): close a file descriptor

close(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, close, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc connect(2): initiate a connection on a socket

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

% @doc connect(2): initiate a connection on a socket

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

% @doc Returns the list of child PIDs for this process

cpid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, cpid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc Returns the list of child PIDs for this process

cpid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, cpid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc environ(7): return the process environment variables

environ(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, environ, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc environ(7): return the process environment variables

environ(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, environ, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc Convert errno integer to atom

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

% @doc Convert errno integer to atom

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

% @doc execve(2): replace process image with environment
% 
% Replace the process image, specifying the environment for the new
% process image.

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

% @doc execve(2): replace process image with environment
% 
% Replace the process image, specifying the environment for the new
% process image.

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

% @doc execvp(2): replace the current process image using the search path

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

% @doc execvp(2): replace the current process image using the search path

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

% @doc exit(3): cause the child process to exit

exit(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, exit, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

% @doc exit(3): cause the child process to exit

exit(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, exit, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc fcntl(2): perform operations on a file descriptor

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

% @doc fcntl(2): perform operations on a file descriptor

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

% @doc Convert fnctl(2) constant to integer

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

% @doc Convert fnctl(2) constant to integer

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

% @doc fexecve(2): replace the process image
% 
% Replace the process image, specifying the environment for the new process
% image, using a previously opened file descriptor.  The file descriptor
% can be set to close after exec() by passing the O_CLOEXEC flag to open:
% 
% ```
% {ok, Proc} = alcove:fork(Drv, []),
% {ok, FD} = alcove:open(Drv, [Proc], "/bin/ls", [o_rdonly,o_cloexec]),
% ok = alcove:fexecve(Drv, [Proc], FD, ["-al"], ["FOO=123"]).
% '''
% 
% Linux and FreeBSD only. Linux requires an environment to be set unlike
% with execve(2). The environment can be empty:
% 
% ```
% % Environment required on Linux
% ok = alcove:fexecve(Drv, [Proc], FD, ["-al"], [""]).
% '''

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

% @doc fexecve(2): replace the process image
% 
% Replace the process image, specifying the environment for the new process
% image, using a previously opened file descriptor.  The file descriptor
% can be set to close after exec() by passing the O_CLOEXEC flag to open:
% 
% ```
% {ok, Proc} = alcove:fork(Drv, []),
% {ok, FD} = alcove:open(Drv, [Proc], "/bin/ls", [o_rdonly,o_cloexec]),
% ok = alcove:fexecve(Drv, [Proc], FD, ["-al"], ["FOO=123"]).
% '''
% 
% Linux and FreeBSD only. Linux requires an environment to be set unlike
% with execve(2). The environment can be empty:
% 
% ```
% % Environment required on Linux
% ok = alcove:fexecve(Drv, [Proc], FD, ["-al"], [""]).
% '''

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

% @doc Constants for open(2)

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

% @doc Constants for open(2)

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

% @doc Restrict available calls for control and subprocess
% 
% filter/4 allows setting different filters for the control and
% subprocesses:
% 
% ```
% ForkOnly = alcove:filter({allow, [fork]}),
% 
% % Process restricted to fork
% % No filter enforced for subprocesses
% ok = alcove:filter(Ctrl, [], ForkOnly, []),
% {ok, Task1} = alcove:fork(Ctrl, []),
% _ = alcove:getpid(Ctrl, [Task1]),
% 
% GetpidOnly = alcove:filter({allow, [getpid]}),
% % []: no change to filter
% ok = alcove:filter(Ctrl, [], [], GetpidOnly),
% {ok, Task2} = alcove:fork(Ctrl, []),
% _ = alcove:getpid(Ctrl, [Task2]).
% '''

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

% @doc Restrict available calls for control and subprocess
% 
% filter/4 allows setting different filters for the control and
% subprocesses:
% 
% ```
% ForkOnly = alcove:filter({allow, [fork]}),
% 
% % Process restricted to fork
% % No filter enforced for subprocesses
% ok = alcove:filter(Ctrl, [], ForkOnly, []),
% {ok, Task1} = alcove:fork(Ctrl, []),
% _ = alcove:getpid(Ctrl, [Task1]),
% 
% GetpidOnly = alcove:filter({allow, [getpid]}),
% % []: no change to filter
% ok = alcove:filter(Ctrl, [], [], GetpidOnly),
% {ok, Task2} = alcove:fork(Ctrl, []),
% _ = alcove:getpid(Ctrl, [Task2]).
% '''

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

% @doc fork(2): create a new process

fork(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, fork, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc fork(2): create a new process

fork(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, fork, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc getcwd(3): return the current working directory

getcwd(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getcwd, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getcwd(3): return the current working directory

getcwd(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getcwd, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc getenv(3): retrieve an environment variable

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

% @doc getenv(3): retrieve an environment variable

getenv(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, getenv, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc getgid(2): retrieve the process group ID

getgid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getgid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getgid(2): retrieve the process group ID

getgid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getgid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc getgroups(2): retrieve the list of supplementary groups

getgroups(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getgroups, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getgroups(2): retrieve the list of supplementary groups

getgroups(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getgroups, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc gethostname(2): retrieve the system hostname

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

% @doc gethostname(2): retrieve the system hostname

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

% @doc Retrieve port options for event loop
% 
% Options are configurable per process, with the default settings inherited
% from the parent.
% 
% The initial values for these options are set for the port by
% alcove_drv:start/1.
% 
% * maxchild : non_neg_integer() : (ulimit -n) / 4 - 4
% 
%   Number of child processes allowed for this process. This value can be
%   modified by adjusting RLIMIT_NOFILE for the process.
% 
% * exit_status : 1 | 0 : 1
% 
%   Controls whether the controlling Erlang process is informed of a
%   process exit value.
% 
% * maxforkdepth : non_neg_integer() : 16
% 
%   Sets the maximum length of the fork chain.
% 
% * termsig : 1 | 0 : 1
% 
%   If a child process exits because of a signal, notify the controlling
%   Erlang process.
% 
% * flowcontrol : int32_t() : -1 (disabled)
% 
%   Sets the default flow control behaviour for a newly forked process. Flow
%   control is applied after the child process calls exec().
% 
%    See setcpid/5.
% 
% * signaloneof : 0-255 : 15
% 
%   Send a signal to a child process on shutdown (stdin of the alcove
%   control process is closed).
% 
%   See setcpid/5.

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

% @doc Retrieve port options for event loop
% 
% Options are configurable per process, with the default settings inherited
% from the parent.
% 
% The initial values for these options are set for the port by
% alcove_drv:start/1.
% 
% * maxchild : non_neg_integer() : (ulimit -n) / 4 - 4
% 
%   Number of child processes allowed for this process. This value can be
%   modified by adjusting RLIMIT_NOFILE for the process.
% 
% * exit_status : 1 | 0 : 1
% 
%   Controls whether the controlling Erlang process is informed of a
%   process exit value.
% 
% * maxforkdepth : non_neg_integer() : 16
% 
%   Sets the maximum length of the fork chain.
% 
% * termsig : 1 | 0 : 1
% 
%   If a child process exits because of a signal, notify the controlling
%   Erlang process.
% 
% * flowcontrol : int32_t() : -1 (disabled)
% 
%   Sets the default flow control behaviour for a newly forked process. Flow
%   control is applied after the child process calls exec().
% 
%    See setcpid/5.
% 
% * signaloneof : 0-255 : 15
% 
%   Send a signal to a child process on shutdown (stdin of the alcove
%   control process is closed).
% 
%   See setcpid/5.

getopt(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, getopt, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc getpgrp(2): retrieve the process group

getpgrp(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getpgrp, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getpgrp(2): retrieve the process group

getpgrp(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getpgrp, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc getpid(2): retrieve the system PID of the process

getpid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getpid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getpid(2): retrieve the system PID of the process

getpid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getpid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc getpriority(2): retrieve scheduling priority of process, process group or user

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

% @doc getpriority(2): retrieve scheduling priority of process, process group or user

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

% @doc getresgid(2): get real, effective and saved group ID
% 
% Supported on Linux and BSDs.

getresgid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getresgid, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getresgid(2): get real, effective and saved group ID
% 
% Supported on Linux and BSDs.

getresgid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getresgid, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc getresuid(2): get real, effective and saved user ID
% 
% Supported on Linux and BSDs.

getresuid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getresuid, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getresuid(2): get real, effective and saved user ID
% 
% Supported on Linux and BSDs.

getresuid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getresuid, [], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc getrlimit(2): retrieve the resource limits for a process
% 
% Returns a record:
% 
% ```
% -include_lib("alcove/include/alcove.hrl").
% 
% #alcove_rlimit{
%     cur = integer(),
%     max = integer()
%     }
% '''

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

% @doc getrlimit(2): retrieve the resource limits for a process
% 
% Returns a record:
% 
% ```
% -include_lib("alcove/include/alcove.hrl").
% 
% #alcove_rlimit{
%     cur = integer(),
%     max = integer()
%     }
% '''

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

% @doc getsid(2): retrieve the session ID

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

% @doc getsid(2): retrieve the session ID

getsid(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, getsid, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc getuid(2): returns the process user ID

getuid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, getuid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc getuid(2): returns the process user ID

getuid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, getuid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc ioctl(2): control device
% 
% Controls a device using a file descriptor previously obtained using
% open/5.
% 
% Argp can be either a binary or a list represention of a C struct. See
% prctl/7 below for a description of the list elements.
% 
% On success, ioctl/5 returns a 3-tuple:
% 
% * Result: an integer equal to the return value of the ioctl
% 
%   Usually 0 but some ioctls may use the return value as the output
%   parameter.
% 
% * Bin: the value depends on the type of the input parameter Argp
% 
% * cstruct: contains the contents of the memory pointed to by Argp
% 
% * integer/binary: an empty binary
% 
% An example of creating a tap device in a net namespace on Linux:
% 
% ```
% {ok, Child} = alcove:clone(Drv, [], [clone_newnet]),
% {ok, FD} = alcove:open(Drv, [Child], "/dev/net/tun", [o_rdwr], 0),
% TUNSETIFF = alcove_ioctl:iow($T, 202, 4),
% {ok, _, <<"tap", N, _/binary>>} = alcove:ioctl(Drv, [Child], FD,
%     TUNSETIFF, <<
%     0:(16*8), % generate a tuntap device name
%     (16#0002 bor 16#1000):2/native-unsigned-integer-unit:8, % IFF_TAP, IFF_NO_PI
%     0:(14*8)
%     >>),
% {ok, <<"tap", N>>}.
% '''

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

% @doc ioctl(2): control device
% 
% Controls a device using a file descriptor previously obtained using
% open/5.
% 
% Argp can be either a binary or a list represention of a C struct. See
% prctl/7 below for a description of the list elements.
% 
% On success, ioctl/5 returns a 3-tuple:
% 
% * Result: an integer equal to the return value of the ioctl
% 
%   Usually 0 but some ioctls may use the return value as the output
%   parameter.
% 
% * Bin: the value depends on the type of the input parameter Argp
% 
% * cstruct: contains the contents of the memory pointed to by Argp
% 
% * integer/binary: an empty binary
% 
% An example of creating a tap device in a net namespace on Linux:
% 
% ```
% {ok, Child} = alcove:clone(Drv, [], [clone_newnet]),
% {ok, FD} = alcove:open(Drv, [Child], "/dev/net/tun", [o_rdwr], 0),
% TUNSETIFF = alcove_ioctl:iow($T, 202, 4),
% {ok, _, <<"tap", N, _/binary>>} = alcove:ioctl(Drv, [Child], FD,
%     TUNSETIFF, <<
%     0:(16*8), % generate a tuntap device name
%     (16#0002 bor 16#1000):2/native-unsigned-integer-unit:8, % IFF_TAP, IFF_NO_PI
%     0:(14*8)
%     >>),
% {ok, <<"tap", N>>}.
% '''

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

% @doc Convert ioctl constant to integer

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

% @doc Convert ioctl constant to integer

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

% @doc Convert iolist to binary
% 
% Test conversion of iolists to binary in a control process.

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

% @doc Convert iolist to binary
% 
% Test conversion of iolists to binary in a control process.

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

% @doc jail(2): restrict the current process in a system jail
% 
% FreeBSD only.
% 
% An example of a function to generate a version 2 jail struct (FreeBSD
% 10.2):
% 
% ```
% struct_jail2(Path, Hostname, Jailname, IPv4, IPv6) ->
%     [
%         <<2:4/native-unsigned-integer-unit:8>>,
%         <<0:(alcove:wordalign(4) * 8)>>,
%         {ptr, <<Path/binary, 0>>},
%         {ptr, <<Hostname/binary, 0>>},
%         {ptr, <<Jailname/binary, 0>>},
%         <<
%             (length(IPv4)):4/native-unsigned-integer-unit:8,
%             (length(IPv6)):4/native-unsigned-integer-unit:8
%         >>,
%         {ptr, <<<<IP1, IP2, IP3, IP4>> || {IP1, IP2, IP3, IP4} <- IPv4>>},
%         {ptr, <<
%             <<IP1:16, IP2:16, IP3:16, IP4:16, IP5:16, IP6:16, IP7:16, IP8:16>>
%          || {IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8} <- IPv6
%         >>}
%     ].
% '''
% 
% To apply the jail:
% 
% ```
% {ok, Child} = alcove:fork(Drv, []),
% Jailv2 = struct_jail2(<<"/rescue">>, <<"test">>, <<"jail0">>, [], []),
% {ok, JID} = alcove:jail(Drv, [Child], Jailv2),
% ok = alcove:chdir(Drv, [Child], "/").
% '''

jail(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, jail, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

% @doc jail(2): restrict the current process in a system jail
% 
% FreeBSD only.
% 
% An example of a function to generate a version 2 jail struct (FreeBSD
% 10.2):
% 
% ```
% struct_jail2(Path, Hostname, Jailname, IPv4, IPv6) ->
%     [
%         <<2:4/native-unsigned-integer-unit:8>>,
%         <<0:(alcove:wordalign(4) * 8)>>,
%         {ptr, <<Path/binary, 0>>},
%         {ptr, <<Hostname/binary, 0>>},
%         {ptr, <<Jailname/binary, 0>>},
%         <<
%             (length(IPv4)):4/native-unsigned-integer-unit:8,
%             (length(IPv6)):4/native-unsigned-integer-unit:8
%         >>,
%         {ptr, <<<<IP1, IP2, IP3, IP4>> || {IP1, IP2, IP3, IP4} <- IPv4>>},
%         {ptr, <<
%             <<IP1:16, IP2:16, IP3:16, IP4:16, IP5:16, IP6:16, IP7:16, IP8:16>>
%          || {IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8} <- IPv6
%         >>}
%     ].
% '''
% 
% To apply the jail:
% 
% ```
% {ok, Child} = alcove:fork(Drv, []),
% Jailv2 = struct_jail2(<<"/rescue">>, <<"test">>, <<"jail0">>, [], []),
% {ok, JID} = alcove:jail(Drv, [Child], Jailv2),
% ok = alcove:chdir(Drv, [Child], "/").
% '''

jail(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, jail, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc jail_attach(2): join a jailed process
% 
% FreeBSD only.

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

% @doc jail_attach(2): join a jailed process
% 
% FreeBSD only.

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

% @doc jail_remove(2): destroy a jailed process
% 
% FreeBSD only.

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

% @doc jail_remove(2): destroy a jailed process
% 
% FreeBSD only.

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

% @doc kill(2): terminate a process

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

% @doc kill(2): terminate a process

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

% @doc link(2) : create a hard link

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

% @doc link(2) : create a hard link

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

% @doc lseek(2): set file offset for read/write

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

% @doc lseek(2): set file offset for read/write

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

% @doc mkdir(2): create a directory

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

% @doc mkdir(2): create a directory

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

% @doc mkfifo(3): make named pipe file

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

% @doc mkfifo(3): make named pipe file

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

% @doc mount(2): mount a filesystem, Linux style
% 
% The arguments are:
% 
% * source
% 
% * target
% 
% * filesystem type
% 
% * flags
% 
% * data
% 
% An empty list may be used to specify NULL.
% 
% For example, filesystems mounted in a Linux mount namespace may be
% visible in the global mount namespace. To avoid this, first remount the
% root filesystem within mount namespace using the MS_REC|MS_PRIVATE flags:
% 
% ```
% {ok, Task} = prx:clone(Parent, [clone_newns]),
% ok = prx:mount(Task, "none", "/", [], [ms_rec, ms_private], []).
% '''
% 
% On BSD systems, the Source argument is ignored and passed to the system
% mount call as:
% 
% ```
% mount(FSType, Target, Flags, Data);
% '''
% 
% On Solaris, some mount options are passed in the Options argument as a
% string of comma separated values terminated by a NULL.  Other platforms
% ignore the Options parameter.

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

% @doc mount(2): mount a filesystem, Linux style
% 
% The arguments are:
% 
% * source
% 
% * target
% 
% * filesystem type
% 
% * flags
% 
% * data
% 
% An empty list may be used to specify NULL.
% 
% For example, filesystems mounted in a Linux mount namespace may be
% visible in the global mount namespace. To avoid this, first remount the
% root filesystem within mount namespace using the MS_REC|MS_PRIVATE flags:
% 
% ```
% {ok, Task} = prx:clone(Parent, [clone_newns]),
% ok = prx:mount(Task, "none", "/", [], [ms_rec, ms_private], []).
% '''
% 
% On BSD systems, the Source argument is ignored and passed to the system
% mount call as:
% 
% ```
% mount(FSType, Target, Flags, Data);
% '''
% 
% On Solaris, some mount options are passed in the Options argument as a
% string of comma separated values terminated by a NULL.  Other platforms
% ignore the Options parameter.

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

% @doc Convert flag names to integers
% 
% The lower case atoms are used for portability:
% 
% ```
% alcove:mount_constant(Drv, [], rdonly)
% '''
% 
% `rdonly' is mapped to MS_RDONLY on Linux and MNT_RDONLY on FreeBSD.

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

% @doc Convert flag names to integers
% 
% The lower case atoms are used for portability:
% 
% ```
% alcove:mount_constant(Drv, [], rdonly)
% '''
% 
% `rdonly' is mapped to MS_RDONLY on Linux and MNT_RDONLY on FreeBSD.

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

% @doc open(2): returns a file descriptor associated with a file
% 
% Lists of values are ORed:
% 
% ```
% alcove:open(Drv, [], "/tmp/test", [o_wronly,o_creat], 8#644)
% '''

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

% @doc open(2): returns a file descriptor associated with a file
% 
% Lists of values are ORed:
% 
% ```
% alcove:open(Drv, [], "/tmp/test", [o_wronly,o_creat], 8#644)
% '''

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

% @doc pivot_root(2): change the root mount
% 
% Linux only.

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

% @doc pivot_root(2): change the root mount
% 
% Linux only.

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

% @doc pledge(2): restrict system operations
% 
% OpenBSD only.
% 
% An empty list ([]) specifies promises should not be changed. Warning:
% an empty string ("") is equivalent to an empty list.
% 
% To specify no capabilities, use an empty binary: `<<>>>' or `<<"">>'
% 
% Example:
% 
% ```
% % no change to restrictions afer exec()
% alcove:pledge(Drv, [Task], <<"stdio proc exec">>, [])
% '''

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

% @doc pledge(2): restrict system operations
% 
% OpenBSD only.
% 
% An empty list ([]) specifies promises should not be changed. Warning:
% an empty string ("") is equivalent to an empty list.
% 
% To specify no capabilities, use an empty binary: `<<>>>' or `<<"">>'
% 
% Example:
% 
% ```
% % no change to restrictions afer exec()
% alcove:pledge(Drv, [Task], <<"stdio proc exec">>, [])
% '''

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

% @doc prctl(2): operations on a process
% 
% Linux only.
% 
% This function can be used to set BPF syscall filters on processes
% (seccomp mode).
% 
% A list can be used for prctl operations requiring a C structure as
% an argument. List elements are used to contiguously populate a buffer
% (it is up to the caller to add padding):
% 
% * binary(): the element is copied directly into the buffer
% 
%   On return, the contents of the binary is returned to the caller.
% 
% * {ptr, N}: N bytes of memory is allocated and zeroed. The pointer is
%   placed in the buffer.
% 
%   On return, the contents of the memory is returned to the caller.
% 
% * {ptr, binary()}
% 
%    Memory equal to the size of the binary is allocated and initialized
%    with the contents of the binary.
% 
%    On return, the contents of the memory is returned to the caller.
% 
% For example, to enforce a seccomp filter:
% 
% ```
% % NOTE: this filter will cause the port to receive a SIGSYS
% % See test/alcove_seccomp_tests.erl for all the syscalls
% % required for the port process to run
% 
% Arch = alcove:define(Drv, [], alcove:audit_arch()),
% Filter = [
%     ?VALIDATE_ARCHITECTURE(Arch),
%     ?EXAMINE_SYSCALL,
%     sys_read,
%     sys_write
% ],
% 
% {ok,_,_,_,_,_} = alcove:prctl(Drv, [], pr_set_no_new_privs, 1, 0, 0, 0),
% Pad = (erlang:system_info({wordsize,external}) - 2) * 8,
% 
% Prog = [
%     <<(iolist_size(Filter) div 8):2/native-unsigned-integer-unit:8>>,
%     <<0:Pad>>,
%     {ptr, list_to_binary(Filter)}
% ],
% alcove:prctl(Drv, [], pr_set_seccomp, seccomp_mode_filter, Prog, 0, 0).
% '''

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

% @doc prctl(2): operations on a process
% 
% Linux only.
% 
% This function can be used to set BPF syscall filters on processes
% (seccomp mode).
% 
% A list can be used for prctl operations requiring a C structure as
% an argument. List elements are used to contiguously populate a buffer
% (it is up to the caller to add padding):
% 
% * binary(): the element is copied directly into the buffer
% 
%   On return, the contents of the binary is returned to the caller.
% 
% * {ptr, N}: N bytes of memory is allocated and zeroed. The pointer is
%   placed in the buffer.
% 
%   On return, the contents of the memory is returned to the caller.
% 
% * {ptr, binary()}
% 
%    Memory equal to the size of the binary is allocated and initialized
%    with the contents of the binary.
% 
%    On return, the contents of the memory is returned to the caller.
% 
% For example, to enforce a seccomp filter:
% 
% ```
% % NOTE: this filter will cause the port to receive a SIGSYS
% % See test/alcove_seccomp_tests.erl for all the syscalls
% % required for the port process to run
% 
% Arch = alcove:define(Drv, [], alcove:audit_arch()),
% Filter = [
%     ?VALIDATE_ARCHITECTURE(Arch),
%     ?EXAMINE_SYSCALL,
%     sys_read,
%     sys_write
% ],
% 
% {ok,_,_,_,_,_} = alcove:prctl(Drv, [], pr_set_no_new_privs, 1, 0, 0, 0),
% Pad = (erlang:system_info({wordsize,external}) - 2) * 8,
% 
% Prog = [
%     <<(iolist_size(Filter) div 8):2/native-unsigned-integer-unit:8>>,
%     <<0:Pad>>,
%     {ptr, list_to_binary(Filter)}
% ],
% alcove:prctl(Drv, [], pr_set_seccomp, seccomp_mode_filter, Prog, 0, 0).
% '''

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

% @doc Convert prctl option names to integers

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

% @doc Convert prctl option names to integers

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

% @doc procctl(2): control processes
% 
% FreeBSD only.

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

% @doc procctl(2): control processes
% 
% FreeBSD only.

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

% @doc ptrace(2): process trace

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

% @doc ptrace(2): process trace

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

% @doc Convert ptrace constant to integer

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

% @doc Convert ptrace constant to integer

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

% @doc read(2): read bytes from a file descriptor

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

% @doc read(2): read bytes from a file descriptor

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

% @doc readdir(3): retrieve list of objects in a directory

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

% @doc readdir(3): retrieve list of objects in a directory

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

% @doc Convert an RLIMIT_* flag to an integer

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

% @doc Convert an RLIMIT_* flag to an integer

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

% @doc rmdir(2): delete a directory

rmdir(Drv, Pids, Arg1) ->
    case alcove_drv:call(Drv, Pids, rmdir, [Arg1], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1]);
        Reply -> Reply
    end.

% @doc rmdir(2): delete a directory

rmdir(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, rmdir, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc seccomp(2): restrict system operations
% 
% Linux only.
% 
% See prctl/7.

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

% @doc seccomp(2): restrict system operations
% 
% Linux only.
% 
% See prctl/7.

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

% @doc Convert seccomp option name to integer

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

% @doc Convert seccomp option name to integer

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

% @doc select(2): poll a list of file descriptor for events
% 
% select/6 will block until an event occurs on a file descriptor, a timeout
% is reached or interrupted by a signal.
% 
% The Timeout value may be:
% 
% * passing an empty list ([]) causes select to block indefinitely
%   (no timeout)
% 
% * an alcove_timeval record with these fields:
% 
%     * sec : number of seconds to wait
% 
%     * usec : number of microseconds to wait

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

% @doc select(2): poll a list of file descriptor for events
% 
% select/6 will block until an event occurs on a file descriptor, a timeout
% is reached or interrupted by a signal.
% 
% The Timeout value may be:
% 
% * passing an empty list ([]) causes select to block indefinitely
%   (no timeout)
% 
% * an alcove_timeval record with these fields:
% 
%     * sec : number of seconds to wait
% 
%     * usec : number of microseconds to wait

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

% @doc Set options for child process of alcove control process
% 
% * flowcontrol
% 
%   Enable rate limiting of the stdout and stderr of a child process. stdin
%   is not rate limited (default: -1 (disabled))
% 
%       * 0 : stdout/stderr for process is not read
%       * 1-2147483646 : read this many messages from the process
%       * -1 : disable flow control
% 
%    NOTE: the limit applies to stdout and stderr. If the limit is set to 1,
%    it is possible to get:
% 
%       * 1 message from stdout
%       * 1 message from stderr
%       * 1 message from stdout and stderr
% 
% * signaloneof
% 
%   The alcove control process sends this signal to the child process on
%   shutdown (default: 15 (SIGTERM))

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

% @doc Set options for child process of alcove control process
% 
% * flowcontrol
% 
%   Enable rate limiting of the stdout and stderr of a child process. stdin
%   is not rate limited (default: -1 (disabled))
% 
%       * 0 : stdout/stderr for process is not read
%       * 1-2147483646 : read this many messages from the process
%       * -1 : disable flow control
% 
%    NOTE: the limit applies to stdout and stderr. If the limit is set to 1,
%    it is possible to get:
% 
%       * 1 message from stdout
%       * 1 message from stderr
%       * 1 message from stdout and stderr
% 
% * signaloneof
% 
%   The alcove control process sends this signal to the child process on
%   shutdown (default: 15 (SIGTERM))

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

% @doc setenv(3): set an environment variable

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

% @doc setenv(3): set an environment variable

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

% @doc setgid(2): set the GID of the process

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

% @doc setgid(2): set the GID of the process

setgid(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, setgid, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc setgroups(2): set the supplementary groups of the process

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

% @doc setgroups(2): set the supplementary groups of the process

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

% @doc sethostname(2): set the system hostname
% 
% This function is probably only useful if running in a uts namespace:
% 
% ```
% {ok, Child} = alcove:clone(Drv, [], [clone_newuts]),
% ok = alcove:sethostname(Drv, [Child], "test"),
% Hostname1 = alcove:gethostname(Drv, []),
% Hostname2 = alcove:gethostname(Drv, [Child]),
% Hostname1 =/= Hostname2.
% '''

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

% @doc sethostname(2): set the system hostname
% 
% This function is probably only useful if running in a uts namespace:
% 
% ```
% {ok, Child} = alcove:clone(Drv, [], [clone_newuts]),
% ok = alcove:sethostname(Drv, [Child], "test"),
% Hostname1 = alcove:gethostname(Drv, []),
% Hostname2 = alcove:gethostname(Drv, [Child]),
% Hostname1 =/= Hostname2.
% '''

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

% @doc setns(2): attach to a namespace
% 
% Linux only.
% 
% A process namespace is represented as a path in the /proc filesystem. The
% path is `/proc/<pid>/ns/<ns>', where:
% 
% * pid: the system PID
% 
% * ns: a file representing the namespace
% 
% The available namespaces is dependent on the kernel version. You can
% see which are supported by running:
% 
% ```
% ls -al /proc/$$/ns
% '''
% 
% For example, to attach to another process network namespace:
% 
% ```
% {ok, Child1} = alcove:clone(Drv, [], [clone_newnet]),
% {ok, Child2} = alcove:fork(Drv, []),
% 
% % Move Child2 into the Child1 network namespace
% {ok,FD} = alcove:open(Drv, [Child2],
%         ["/proc/", integer_to_list(Child1), "/ns/net"], [o_rdonly], 0),
% ok = alcove:setns(Drv, [Child2], FD, 0),
% ok = alcove:close(Drv, [Child2], FD).
% '''

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

% @doc setns(2): attach to a namespace
% 
% Linux only.
% 
% A process namespace is represented as a path in the /proc filesystem. The
% path is `/proc/<pid>/ns/<ns>', where:
% 
% * pid: the system PID
% 
% * ns: a file representing the namespace
% 
% The available namespaces is dependent on the kernel version. You can
% see which are supported by running:
% 
% ```
% ls -al /proc/$$/ns
% '''
% 
% For example, to attach to another process network namespace:
% 
% ```
% {ok, Child1} = alcove:clone(Drv, [], [clone_newnet]),
% {ok, Child2} = alcove:fork(Drv, []),
% 
% % Move Child2 into the Child1 network namespace
% {ok,FD} = alcove:open(Drv, [Child2],
%         ["/proc/", integer_to_list(Child1), "/ns/net"], [o_rdonly], 0),
% ok = alcove:setns(Drv, [Child2], FD, 0),
% ok = alcove:close(Drv, [Child2], FD).
% '''

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

% @doc Set port options
% 
% See getopt/2,3 for the list of options.

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

% @doc Set port options
% 
% See getopt/2,3 for the list of options.

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

% @doc setpgid(2): set process group

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

% @doc setpgid(2): set process group

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

% @doc setpriority(2): set scheduling priority of process, process group or user

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

% @doc setpriority(2): set scheduling priority of process, process group or user

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

% @doc setproctitle(3): set the process title
% 
% On Linux, prctl/6,7 can also be used:
% 
% ```
% {ok,Fork} = alcove:fork(Drv, []),
% alcove:prctl(Drv, [Fork], pr_set_name, <<"pseudonym">>, 0,0,0).
% '''

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

% @doc setproctitle(3): set the process title
% 
% On Linux, prctl/6,7 can also be used:
% 
% ```
% {ok,Fork} = alcove:fork(Drv, []),
% alcove:prctl(Drv, [Fork], pr_set_name, <<"pseudonym">>, 0,0,0).
% '''

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

% @doc setresgid(2): set real, effective and saved group ID
% 
% Supported on Linux and BSDs.

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

% @doc setresgid(2): set real, effective and saved group ID
% 
% Supported on Linux and BSDs.

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

% @doc setresuid(2): set real, effective and saved user ID
% 
% Supported on Linux and BSDs.

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

% @doc setresuid(2): set real, effective and saved user ID
% 
% Supported on Linux and BSDs.

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

% @doc setrlimit(2): set a resource limit

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

% @doc setrlimit(2): set a resource limit

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

% @doc setsid(2): create a new session

setsid(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, setsid, [], infinity) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc setsid(2): create a new session

setsid(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, setsid, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc setuid(2): change UID

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

% @doc setuid(2): change UID

setuid(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, setuid, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc sigaction(2): set process behaviour for signals
% 
% * sig_dfl
% 
%   Uses the default behaviour for the signal
% 
% * sig_ign
% 
%   Ignores the signal
% 
% * sig_info
% 
%   Catches the signal and sends the controlling Erlang process an event:
% 
% ```
% {signal, atom(), Info}
% '''
% 
%   Info is a binary containing the siginfo_t structure. See sigaction(2)
%   for details.
% 
% * []
% 
%   Returns the current handler for the signal.
% 
% Multiple caught signals of the same type may be reported as one event.

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

% @doc sigaction(2): set process behaviour for signals
% 
% * sig_dfl
% 
%   Uses the default behaviour for the signal
% 
% * sig_ign
% 
%   Ignores the signal
% 
% * sig_info
% 
%   Catches the signal and sends the controlling Erlang process an event:
% 
% ```
% {signal, atom(), Info}
% '''
% 
%   Info is a binary containing the siginfo_t structure. See sigaction(2)
%   for details.
% 
% * []
% 
%   Returns the current handler for the signal.
% 
% Multiple caught signals of the same type may be reported as one event.

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

% @doc Convert signal names to integers

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

% @doc Convert signal names to integers

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

% @doc socket(2): returns a file descriptor for a communication endpoint

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

% @doc socket(2): returns a file descriptor for a communication endpoint

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

% @doc symlink(2): create a symbolic link

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

% @doc symlink(2): create a symbolic link

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

% @doc Convert syscall name to integer

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

% @doc Convert syscall name to integer

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

% @doc umount(2): unmount a filesystem
% 
% On BSD systems, calls unmount(2).

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

% @doc umount(2): unmount a filesystem
% 
% On BSD systems, calls unmount(2).

umount(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, umount, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc umount2(2): unmount filesystem with flags
% 
% Linux only.

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

% @doc umount2(2): unmount filesystem with flags
% 
% Linux only.

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

% @doc unlink(2): delete a name from the filesystem

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

% @doc unlink(2): delete a name from the filesystem

unlink(Drv, Pids, Arg1, Timeout) ->
    case alcove_drv:call(Drv, Pids, unlink, [Arg1], Timeout)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Arg1, Timeout]);
        Reply -> Reply
    end.

% @doc unsetenv(3): remove an environment variable

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

% @doc unsetenv(3): remove an environment variable

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

% @doc unshare(2): create a new namespace in the current process
% 
% Make a new namespace without calling clone(2):
% 
% ```
% ok = alcove:unshare(Drv, [], [clone_newnet]).
% % The port is now running in a namespace without network access.
% '''

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

% @doc unshare(2): create a new namespace in the current process
% 
% Make a new namespace without calling clone(2):
% 
% ```
% ok = alcove:unshare(Drv, [], [clone_newnet]).
% % The port is now running in a namespace without network access.
% '''

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

% @doc unveil(2): restrict filesystem view
% 
% OpenBSD only.
% 
% To disable unveil calls, use an empty list ([]) or, equivalently, an
% empty string ("").
% 
% ```
% alcove:unveil(Drv, [Task], <<"/etc">>, <<"r">>),
% alcove:unveil(Drv, [Task], [], []).
% '''

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

% @doc unveil(2): restrict filesystem view
% 
% OpenBSD only.
% 
% To disable unveil calls, use an empty list ([]) or, equivalently, an
% empty string ("").
% 
% ```
% alcove:unveil(Drv, [Task], <<"/etc">>, <<"r">>),
% alcove:unveil(Drv, [Task], [], []).
% '''

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

% @doc Retrieve the alcove version

version(Drv, Pids) ->
    case alcove_drv:call(Drv, Pids, version, [], infinity)
        of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids]);
        Reply -> Reply
    end.

% @doc Retrieve the alcove version

version(Drv, Pids, Timeout) ->
    case alcove_drv:call(Drv, Pids, version, [], Timeout) of
        {alcove_error, Error} ->
            erlang:error(Error, [Drv, Pids, Timeout]);
        Reply -> Reply
    end.

% @doc waitpid(2): wait for process to change state

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

% @doc waitpid(2): wait for process to change state

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

% @doc write(2): write to a file descriptor
% 
% Writes a buffer to a file descriptor and returns the number of bytes
% written.

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

% @doc write(2): write to a file descriptor
% 
% Writes a buffer to a file descriptor and returns the number of bytes
% written.

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