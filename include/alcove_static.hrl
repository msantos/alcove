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

-spec alloc(alcove_drv:ref(), [pid_t()], Ptr :: cstruct()) -> {'ok', binary(), iodata()}.
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

-type waitstatus() ::
    {exit_status, int32_t()}
    | {termsig, atom()}
    | {stopsig, atom()}
    | continued.
-spec waitpid(alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), Options :: int32_t() | [constant()]) ->
    {'ok', pid_t(), WaitStatus :: [waitstatus()]} | {'error', posix()}.
-spec waitpid(
    alcove_drv:ref(), [pid_t()], OSPid :: pid_t(), Options :: int32_t() | [constant()], timeout()
) ->
    {'ok', pid_t(), WaitStatus :: [waitstatus()]} | {'error', posix()}.

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
%
% == Examples ==
%
% ```
% 1> {ok, Drv} = alcove_drv:start().
% {ok,<0.176.0>}
% 2> alcove:audit_arch().
% audit_arch_x86_64
% 3> alcove:define(Drv, [], alcove:audit_arch()).
% 3221225534
% '''
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
%
% == Examples ==
%
% ```
% 1> {ok, Drv} = alcove_drv:start().
% {ok,<0.176.0>}
% 2> alcove:define(Drv, [], clone_newns).
% 131072
% '''
-spec define(alcove_drv:ref(), [pid_t()], atom() | [atom()]) -> integer().
define(Drv, Pipeline, Constant) when is_atom(Constant) ->
    define(Drv, Pipeline, [Constant]);
define(Drv, Pipeline, Constants) when is_list(Constants) ->
    lists:foldl(
        fun
            (Constant, Result) when is_atom(Constant) ->
                Val = define_constant(Drv, Pipeline, Constant),
                Result bxor Val;
            (Val, Result) when is_integer(Val) ->
                Result bxor Val
        end,
        0,
        Constants
    ).

define_constant(Drv, Pipeline, Constant) ->
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
    define_foreach(Drv, Pipeline, Constant, Fun).

define_foreach(_Drv, _Pipeline, Constant, []) ->
    erlang:error({unknown, Constant});
define_foreach(Drv, Pipeline, Constant, [Fun | Rest]) ->
    try Fun(Drv, Pipeline, Constant) of
        unknown ->
            define_foreach(Drv, Pipeline, Constant, Rest);
        Val when is_integer(Val) ->
            Val
    catch
        % Function call not supported on this platform
        error:undef ->
            define_foreach(Drv, Pipeline, Constant, Rest)
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
