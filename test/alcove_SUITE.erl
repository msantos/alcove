%%% Copyright (c) 2014-2021, Michael Santos <michael.santos@gmail.com>
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
-module(alcove_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("alcove/include/alcove.hrl").
-include_lib("alcove/include/alcove_seccomp.hrl").

-export([
    suite/0,
    all/0,
    groups/0,
    init_per_testcase/2,
    end_per_testcase/2
]).

-export([
    alloc/1,
    badpid/1,
    cap_enter/1,
    cap_fcntls_limit/1,
    cap_ioctls_limit/1,
    cap_rights_limit/1,
    chdir/1,
    chmod/1,
    chroot/1,
    clone_constant/1,
    connect/1,
    cpid/1,
    env/1,
    eof/1,
    errno_id/1,
    event/1,
    execve/1,
    execvp/1,
    execvp_mid_chain/1,
    execvp_with_signal/1,
    fcntl/1,
    fexecve/1,
    fexecve_sigchld/1,
    file_constant/1,
    filter/1,
    flowcontrol/1,
    flowcontrol_fork_exec_exit/1,
    fork/1,
    forkchain/1,
    forkstress/1,
    getpid/1,
    ioctl/1,
    ioctl_constant/1,
    iodata/1,
    jail/1,
    mkfifo/1,
    mount/1,
    mount_constant/1,
    open/1,
    pipe_buf/1,
    pledge/1,
    portstress/1,
    prctl/1,
    prctl_constant/1,
    priority/1,
    process_tree_leader_exits/1,
    ptrace/1,
    ptrace_constant/1,
    rlimit_constant/1,
    select/1,
    setgid/1,
    setgroups/1,
    sethostname/1,
    setns/1,
    setopt/1,
    setrlimit/1,
    setuid/1,
    signal/1,
    signal_constant/1,
    signaloneof/1,
    socket/1,
    stderr/1,
    stdout/1,
    stream/1,
    symlink/1,
    syscall_constant/1,
    tmpfs/1,
    unshare/1,
    unveil/1,
    version/1,

    no_os_specific_tests/1
]).

suite() ->
    Timeout = list_to_integer(os:getenv("ALCOVE_TEST_TIMEOUT", "30")),
    [{timetrap, {seconds, Timeout}}].

all() ->
    {unix, OS} = os:type(),
    [
        {group, OS},
        process_tree_leader_exits,
        version,
        iodata,
        file_constant,
        ioctl_constant,
        rlimit_constant,
        signal_constant,
        errno_id,
        cpid,
        getpid,
        setopt,
        event,
        sethostname,
        env,
        mount_constant,
        mount,
        tmpfs,
        chroot,
        chdir,
        setrlimit,
        setgid,
        setuid,
        chmod,
        fork,
        badpid,
        signal,
        portstress,
        forkstress,
        forkchain,
        eof,
        alloc,
        priority,
        fcntl,
        execvp,
        execvp_with_signal,
        stdout,
        stderr,
        execve,
        stream,
        open,
        socket,
        select,
        connect,
        mkfifo,
        ioctl,
        symlink,
        execvp_mid_chain,
        pipe_buf,
        signaloneof,
        flowcontrol,
        flowcontrol_fork_exec_exit,
        filter
    ].

groups() ->
    [
        {linux, [sequence], [
            setgroups,
            fexecve,
            fexecve_sigchld,
            clone_constant,
            prctl_constant,
            ptrace_constant,
            syscall_constant,
            setns,
            unshare,
            prctl,
            ptrace
        ]},
        {freebsd, [sequence], [
            setgroups,
            fexecve,
            fexecve_sigchld,
            jail,
            cap_enter,
            cap_rights_limit,
            cap_fcntls_limit,
            cap_ioctls_limit
        ]},
        {openbsd, [], [setgroups, pledge, unveil]},
        {darwin, [], [no_os_specific_tests]},
        {netbsd, [], [setgroups]},
        {solaris, [], [setgroups]}
    ].

init_per_testcase(_Test, Config) ->
    % export ALCOVE_TEST_EXEC="sudo valgrind --leak-check=yes --log-file=/tmp/alcove.log"
    Exec = getenv("ALCOVE_TEST_EXEC", "sudo -n"),
    Use_fork = false =/= getenv("ALCOVE_TEST_USE_FORK", false),

    Ctldir =
        case getenv("ALCOVE_TEST_CTLDIR", false) of
            false -> [];
            Dir -> [{ctldir, Dir}]
        end,

    {ok, Drv} = alcove_drv:start_link([{exec, Exec}, {maxchild, 8}] ++ Ctldir),

    case {Use_fork, os:type()} of
        {false, {unix, linux}} ->
            {ok, Child} = alcove:clone(Drv, [], [
                clone_newipc,
                clone_newnet,
                clone_newns,
                clone_newpid,
                clone_newuts
            ]),
            [
                {drv, Drv},
                {child, Child},
                {namespace, true},
                {os, linux}
                | Config
            ];
        {_, {unix, OS}} ->
            {ok, Child} = alcove:fork(Drv, []),
            [
                {drv, Drv},
                {child, Child},
                {namespace, false},
                {os, OS}
                | Config
            ]
    end.

end_per_testcase(connect, Config) ->
    Drv = ?config(drv, Config),
    alcove_drv:stop(Drv);
end_per_testcase(_Test, Config) ->
    Drv = ?config(drv, Config),
    alcove_drv:stop(Drv).

%%
%% Tests
%%
version(Config) ->
    Drv = ?config(drv, Config),
    Version = alcove:version(Drv, []),
    true = is_binary(Version).

iodata(Config) ->
    Drv = ?config(drv, Config),
    Iolist = [
        "0",
        1,
        "2",
        3,
        [4, [5, 6, <<7, 8, 9>>, ["10"]]],
        <<"1112, 13, 14, 15">>,
        [16, 17, "18,19,20"],
        21,
        <<22>>
    ],

    Bin = alcove:iolist_to_bin(Drv, [], Iolist),
    Bin = iolist_to_binary(Iolist),

    % Valid iolists: binary, string, lists, bytes must be within a list
    {'EXIT', {badarg, _}} = (catch alcove:iolist_to_bin(Drv, [], 10)),
    {'EXIT', {badarg, _}} = (catch alcove:iolist_to_bin(Drv, [], [123456])),
    <<1, 2, 3, 4, 5, 6>> = alcove:iolist_to_bin(Drv, [], <<1, 2, 3, 4, 5, 6>>),
    <<"ok">> = alcove:iolist_to_bin(Drv, [], "ok"),

    % Arbitrary implementation limit of 16 nested
    % lists. iolist_to_binary/1 does not have this limitation.
    <<"ok">> = alcove:iolist_to_bin(
        Drv,
        [],
        [[[[[[[[[[[[[[[["ok"]]]]]]]]]]]]]]]]
    ),

    {'EXIT', {badarg, _}} =
        (catch alcove:iolist_to_bin(
            Drv,
            [],
            [[[[[[[[[[[[[[[[["fail"]]]]]]]]]]]]]]]]]
        )),

    ok.

file_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    O_RDONLY = alcove:file_constant(Drv, [], 'O_RDONLY'),
    O_RDONLY = alcove:file_constant(Drv, [Child], o_rdonly),
    true = is_integer(O_RDONLY),

    unknown = alcove:file_constant(Drv, [], nonexist),

    ok.

ioctl_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    SIOCGIFADDR = alcove:ioctl_constant(Drv, [], 'SIOCGIFADDR'),
    SIOCGIFADDR = alcove:ioctl_constant(Drv, [Child], siocgifaddr),
    true = is_integer(SIOCGIFADDR),

    unknown = alcove:ioctl_constant(Drv, [], nonexist),

    ok.

prctl_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    PR_SET_NAME = alcove:prctl_constant(Drv, [], 'PR_SET_NAME'),
    PR_SET_NAME = alcove:prctl_constant(Drv, [Child], pr_set_name),
    true = is_integer(PR_SET_NAME),

    unknown = alcove:prctl_constant(Drv, [], nonexist),

    ok.

ptrace_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    PTRACE_TRACEME = alcove:ptrace_constant(Drv, [], 'PTRACE_TRACEME'),
    PTRACE_TRACEME = alcove:ptrace_constant(Drv, [Child], ptrace_traceme),
    true = is_integer(PTRACE_TRACEME),

    unknown = alcove:ptrace_constant(Drv, [], nonexist),

    ok.

rlimit_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    RLIMIT_NOFILE = alcove:rlimit_constant(Drv, [], 'RLIMIT_NOFILE'),
    RLIMIT_NOFILE = alcove:rlimit_constant(Drv, [Child], rlimit_nofile),
    true = is_integer(RLIMIT_NOFILE),

    unknown = alcove:rlimit_constant(Drv, [], nonexist),

    ok.

signal_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    SIGHUP = alcove:signal_constant(Drv, [], 'SIGHUP'),
    SIGHUP = alcove:signal_constant(Drv, [Child], sighup),
    true = is_integer(SIGHUP),

    unknown = alcove:signal_constant(Drv, [], nonexist),

    ok.

syscall_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    N = alcove:syscall_constant(Drv, [], 'SYS_exit'),
    N = alcove:syscall_constant(Drv, [Child], sys_exit),
    true = is_integer(N),

    unknown = alcove:syscall_constant(Drv, [], nonexist),

    ok.

errno_id(Config) ->
    Drv = ?config(drv, Config),

    eperm = alcove:errno_id(Drv, [], 1),
    unknown = alcove:errno_id(Drv, [], 0),

    ok.

cpid(Config) ->
    Drv = ?config(drv, Config),

    {ok, Child} = alcove:fork(Drv, []),
    {ok, Grandchild} = alcove:fork(Drv, [Child]),
    [#alcove_pid{}] = alcove:cpid(Drv, [Child]),
    ok = alcove:execvp(Drv, [Child, Grandchild], "/bin/cat", ["/bin/cat"]),
    [#alcove_pid{fdctl = -2}] = alcove:cpid(Drv, [Child]),
    ok = alcove:eof(Drv, [Child]).

getpid(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    NS = ?config(namespace, Config),

    case NS of
        true ->
            % Running in a PID namespace
            1 = alcove:getpid(Drv, [Child]);
        false ->
            PID = alcove:getpid(Drv, [Child]),
            true = PID > 0
    end.

setopt(Config) ->
    Drv = ?config(drv, Config),

    {ok, Fork} = alcove:fork(Drv, []),

    true = alcove:setopt(Drv, [Fork], maxforkdepth, 128),

    {ok, Fork1} = alcove:fork(Drv, [Fork]),

    128 = alcove:getopt(Drv, [Fork], maxforkdepth),
    128 = alcove:getopt(Drv, [Fork, Fork1], maxforkdepth),

    alcove:exit(Drv, [Fork, Fork1], 0),

    true = alcove:setopt(Drv, [Fork], exit_status, 0),
    true = alcove:setopt(Drv, [Fork], maxforkdepth, 0),

    0 = alcove:getopt(Drv, [Fork], exit_status),
    true = 0 =/= alcove:getopt(Drv, [], maxforkdepth),
    0 = alcove:getopt(Drv, [Fork], maxforkdepth),
    {error, eagain} = alcove:fork(Drv, [Fork]).

event(Config) ->
    Drv = ?config(drv, Config),

    {ok, _} = alcove:sigaction(Drv, [], sigchld, sig_info),

    {ok, Fork} = alcove:fork(Drv, []),
    ok = alcove:exit(Drv, [Fork], 0),
    {ok, Fork, 0, [{exit_status, 0}]} = alcove:waitpid(Drv, [], -1, 0),
    {signal, _, _} = alcove:event(Drv, [], 5000).

sethostname(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    NS = ?config(namespace, Config),

    case NS of
        true ->
            ok = alcove:sethostname(Drv, [Child], "alcove"),
            {ok, <<"alcove">>} = alcove:gethostname(Drv, [Child]);
        false ->
            {ok, Hostname} = alcove:gethostname(Drv, [Child]),
            true = is_binary(Hostname)
    end.

env(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    false = alcove:getenv(Drv, [Child], "ALCOVE"),
    ok = alcove:setenv(Drv, [Child], "ALCOVE", "12345", 0),
    <<"12345">> = alcove:getenv(Drv, [Child], "ALCOVE"),
    ok = alcove:setenv(Drv, [Child], "ALCOVE", "abcd", 1),
    <<"abcd">> = alcove:getenv(Drv, [Child], "ALCOVE"),
    ok = alcove:unsetenv(Drv, [Child], "ALCOVE"),
    false = alcove:getenv(Drv, [Child], "ALCOVE"),

    true = 0 =/= length(alcove:environ(Drv, [Child])),
    ok = alcove:clearenv(Drv, [Child]),
    true = 0 =:= length(alcove:environ(Drv, [Child])).

mount_constant(Config) ->
    Drv = ?config(drv, Config),
    OS = ?config(os, Config),

    case OS of
        sunos ->
            Flags = alcove:define(Drv, [], [
                rdonly,
                nosuid
            ]),
            true = is_integer(Flags);
        _ ->
            Flags = alcove:define(Drv, [], [
                rdonly,
                nosuid,
                noexec,
                noatime
            ]),
            true = is_integer(Flags)
    end.

mount(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    NS = ?config(namespace, Config),

    case NS of
        true ->
            ok = alcove:mount(
                Drv,
                [Child],
                "/tmp",
                "/mnt",
                "",
                [
                    ms_bind,
                    ms_rdonly,
                    ms_noexec
                ],
                "",
                ""
            ),
            ok = alcove:umount(Drv, [Child], "/mnt"),

            ok = alcove:mount(
                Drv,
                [Child],
                "/tmp",
                "/mnt",
                "",
                [
                    ms_bind,
                    ms_rdonly,
                    ms_noexec
                ],
                "",
                ""
            ),
            ok = alcove:umount2(Drv, [Child], "/mnt", [mnt_detach]);
        false ->
            {skip, "mount namespaces not supported"}
    end.

tmpfs(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    NS = ?config(namespace, Config),
    OS = ?config(os, Config),

    case {OS, NS} of
        {_, true} ->
            ok = alcove:mount(
                Drv,
                [Child],
                "tmpfs",
                "/mnt",
                "tmpfs",
                [ms_noexec],
                <<"size=16M", 0>>,
                <<>>
            ),
            ok = alcove:umount(Drv, [Child], "/mnt");
        {N, false} when N =:= linux ->
            % Linux: running in a fork in the global namespace
            Dir = "/tmp/alcove." ++ os:getpid(),
            ok = alcove:mkdir(Drv, [Child], Dir, 8#700),
            ok = alcove:mount(
                Drv,
                [Child],
                "tmpfs",
                Dir,
                "tmpfs",
                [noexec],
                <<"size=16M", 0>>,
                []
            ),
            ok = alcove:umount(Drv, [Child], Dir),
            ok = alcove:rmdir(Drv, [Child], Dir);
        {sunos, false} ->
            Dir = "/tmp/alcove." ++ os:getpid(),
            ok = alcove:mkdir(Drv, [Child], Dir, 8#700),
            ok = alcove:mount(
                Drv,
                [Child],
                "swap",
                Dir,
                "tmpfs",
                [ms_optionstr],
                [],
                <<"size=16m", 0:4096>>
            ),
            ok = alcove:umount(Drv, [Child], Dir),
            ok = alcove:rmdir(Drv, [Child], Dir);
        _ ->
            {skip, "no tmpfs test defined for this platform"}
    end.

chroot(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    OS = ?config(os, Config),

    case OS of
        N when N =:= linux; N =:= openbsd ->
            ok = alcove:chroot(Drv, [Child], "/bin");
        netbsd ->
            ok = alcove:chroot(Drv, [Child], "/rescue");
        freebsd ->
            ok = alcove:chroot(Drv, [Child], "/rescue");
        _ ->
            {skip, "no chroot test defined for this platform"}
    end.

chdir(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:chdir(Drv, [Child], "/"),
    {ok, <<"/">>} = alcove:getcwd(Drv, [Child]).

chmod(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    OS = ?config(os, Config),

    ok = alcove:setuid(Drv, [Child], 65534),

    Dir = "/tmp/alcove-chmod." ++ os:getpid(),

    ok = alcove:mkdir(Drv, [Child], Dir, 8#700),
    {ok, _} = alcove:readdir(Drv, [Child], Dir),
    ok = alcove:chmod(Drv, [Child], Dir, 8#000),
    {error, eacces} = alcove:readdir(Drv, [Child], Dir),
    ok = alcove:chmod(Drv, [Child], Dir, 8#700),

    ok = alcove:chown(Drv, [], Dir, 0, 0),
    Reply = alcove:rmdir(Drv, [Child], Dir),
    Reply =
        case OS of
            darwin ->
                {error, eacces};
            _ ->
                {error, eperm}
        end,

    ok = alcove:chown(Drv, [], Dir, 65534, 65534),

    ok = alcove:rmdir(Drv, [Child], Dir).

setrlimit(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:setrlimit(Drv, [Child], rlimit_nofile, #alcove_rlimit{
        cur = 64,
        max = 64
    }),
    {ok, #alcove_rlimit{cur = 64, max = 64}} = alcove:getrlimit(
        Drv,
        [Child],
        rlimit_nofile
    ).

setgid(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:setgid(Drv, [Child], 65534),
    65534 = alcove:getgid(Drv, [Child]).

setuid(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:setuid(Drv, [Child], 65534),
    65534 = alcove:getuid(Drv, [Child]).

setgroups(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    OS = ?config(os, Config),

    Groups = [10000, 10001, 10002],

    ok = alcove:setgroups(Drv, [Child], Groups),
    {ok, Reply0} = alcove:getgroups(Drv, [Child]),
    Groups = lists:sort(Reply0),

    ok = alcove:setgroups(Drv, [Child], []),
    {ok, Reply1} = alcove:getgroups(Drv, [Child]),

    Reply1 =
        case OS of
            freebsd ->
                [hd(Groups)];
            _ ->
                []
        end.

fork(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    {ok, RL} = alcove:getrlimit(Drv, [Child], rlimit_nofile),
    ok = alcove:setrlimit(
        Drv,
        [Child],
        rlimit_nofile,
        RL#alcove_rlimit{cur = 64}
    ),
    4 = alcove:getopt(Drv, [Child], maxchild),

    Pids = [alcove:fork(Drv, [Child]) || _ <- lists:seq(1, 32)],
    [{error, eagain} | _Rest] = lists:reverse(Pids),
    Reply0 = alcove:getpid(Drv, [Child]),
    true = is_integer(Reply0),

    [{ok, Child0} | _] = [N || N <- Pids, N =/= {error, eagain}],
    ok = alcove:exit(Drv, [Child, Child0], 0),
    {exit_status, 0} = alcove:event(Drv, [Child, Child0], 5000),
    {ok, _} = alcove:fork(Drv, [Child]),
    {error, eagain} = alcove:fork(Drv, [Child]).

badpid(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    % EPIPE or PID not found
    ok = alcove:execvp(
        Drv,
        [Child],
        "/bin/sh",
        ["/bin/sh", "-c", "echo > /dev/null"]
    ),
    {exit_status, 0} = alcove:event(Drv, [Child], 5000),
    {'EXIT', {badpid, _}} =
        (catch alcove:execvp(
            Drv,
            [Child],
            "/bin/sh",
            ["/bin/sh", "-c", "echo > /dev/null"]
        )),

    % PID not found
    PID = get_unused_pid(Drv),
    {'EXIT', {badpid, _}} =
        (catch alcove:execvp(
            Drv,
            [PID],
            "/bin/sh",
            ["/bin/sh", "-c", "echo > /dev/null"]
        )),

    % Invalid PIDs
    {'EXIT', {badpid, _}} =
        (catch alcove:execvp(
            Drv,
            [0],
            "/bin/sh",
            ["/bin/sh", "-c", "echo > /dev/null"]
        )),

    ok.

signal(Config) ->
    Drv = ?config(drv, Config),

    % Linux: cannot be PID 1 in a namespace. PID 1 can't be killed.
    {ok, Child} = alcove:fork(Drv, []),

    {ok, sig_info} = alcove:sigaction(Drv, [Child], sigterm, sig_ign),
    {ok, sig_ign} = alcove:sigaction(Drv, [Child], sigterm, []),
    ok = alcove:kill(Drv, [], Child, sigterm),
    Pid0 = alcove:getpid(Drv, [Child]),
    true = is_integer(Pid0),

    {ok, sig_ign} = alcove:sigaction(Drv, [Child], sigterm, sig_dfl),
    {ok, sig_dfl} = alcove:sigaction(Drv, [Child], sigterm, []),
    ok = alcove:kill(Drv, [], Child, sigterm),
    {termsig, sigterm} = alcove:event(Drv, [Child], 5000),
    alcove:kill(Drv, [], Child, 0),
    {error, esrch} = alcove:kill(Drv, [], Child, 0).

portstress(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    Versions = [alcove:version(Drv, [Child]) || _ <- lists:seq(1, 1000)],
    Versions = lists:filter(
        fun
            (false) -> false;
            (_) -> true
        end,
        Versions
    ).

forkstress(Config) ->
    Drv = ?config(drv, Config),

    {ok, Fork} = alcove:fork(Drv, []),
    ok = forkstress_1(Drv, Fork, 100).

forkchain(Config) ->
    Drv = ?config(drv, Config),

    {ok, Child0} = alcove:fork(Drv, []),
    {ok, Child1} = alcove:fork(Drv, [Child0]),
    {ok, Child2} = alcove:fork(Drv, [Child0, Child1]),
    {ok, Child3} = alcove:fork(Drv, [Child0, Child1, Child2]),
    {ok, Child4} = alcove:fork(Drv, [Child0, Child1, Child2, Child3]),

    Child4 = alcove:getpid(Drv, [Child0, Child1, Child2, Child3, Child4]),

    ok = alcove:exit(Drv, [Child0], 0).

eof(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    {ok, Child0} = alcove:fork(Drv, [Child]),

    ok = alcove:eof(Drv, [Child, Child0], stderr),
    {error, ebadf} = alcove:eof(Drv, [Child, Child0], stderr),

    ok = alcove:eof(Drv, [Child, Child0], stdout),
    {error, ebadf} = alcove:eof(Drv, [Child, Child0], stdout),

    ok = alcove:eof(Drv, [Child, Child0]),
    ok =
        case alcove:eof(Drv, [Child, Child0]) of
            {error, esrch} -> ok;
            {error, ebadf} -> ok;
            N -> N
        end.

alloc(Config) ->
    Drv = ?config(drv, Config),

    {ok, Buf, Cstruct} = alcove:alloc(
        Drv,
        [],
        [
            <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>>,
            {ptr, 11},
            <<11, 12, 13, 14, 15>>,
            {ptr, <<16, 17, 18, 19, 20, 21, 22>>},
            <<23, 24, 25>>
        ]
    ),
    Size = erlang:system_info({wordsize, external}),
    Buflen = 10 + Size + 5 + Size + 3,
    Buflen = byte_size(Buf),

    <<1, 2, 3, 4, 5, 6, 7, 8, 9, 10>> = lists:nth(1, Cstruct),
    {ptr, <<0:88>>} = lists:nth(2, Cstruct),
    <<11, 12, 13, 14, 15>> = lists:nth(3, Cstruct),
    {ptr, <<16, 17, 18, 19, 20, 21, 22>>} = lists:nth(4, Cstruct),
    <<23, 24, 25>> = lists:nth(5, Cstruct),

    Bits = Size * 8,

    {'EXIT', {badarg, _}} = (catch alcove:alloc(Drv, [], [])),
    {ok, <<>>, [<<>>]} = alcove:alloc(Drv, [], [<<>>]),
    {ok, <<>>, [<<>>, <<>>]} = alcove:alloc(Drv, [], [<<>>, <<>>]),
    {ok, <<>>, [<<>>, <<>>, <<>>]} = alcove:alloc(Drv, [], [<<>>, <<>>, <<>>]),
    {ok, <<0:Bits>>, [<<0:Bits>>]} = alcove:alloc(Drv, [], [{ptr, 0}]),
    {ok, <<0:Bits>>, [<<0:Bits>>]} = alcove:alloc(Drv, [], [{ptr, <<>>}]),

    {ok, BufWithNulls, CstructWithNulls} = alcove:alloc(Drv, [], [
        <<>>,
        {ptr, 0},
        <<"foo123">>,
        {ptr, <<>>},
        <<"bar456789">>
    ]),

    BuflenWithNulls = 0 + Size + 6 + Size + 9,
    BuflenWithNulls = byte_size(BufWithNulls),

    <<>> = lists:nth(1, CstructWithNulls),
    <<0:Bits>> = lists:nth(2, CstructWithNulls),
    <<"foo123">> = lists:nth(3, CstructWithNulls),
    <<0:Bits>> = lists:nth(4, CstructWithNulls),
    <<"bar456789">> = lists:nth(5, CstructWithNulls).

prctl(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    % capability is set:
    %   returns 0 | 1 in function result, arg2 = int
    {ok, 1, 0, 0, 0, 0} = alcove:prctl(Drv, [Child], pr_capbset_read, 0, 0, 0, 0),

    % set process name:
    %   arg2 = char *, up to 16 bytes, NULL terminated
    {ok, 0, <<116, 101, 115, 116, 0>>, 0, 0, 0} = alcove:prctl(
        Drv,
        [Child],
        pr_set_name,
        <<"test", 0>>,
        0,
        0,
        0
    ),

    % get process name
    %   value returned in arg2 = char *, up to 16 bytes
    {ok, 0, <<116, 101, 115, 116, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>, 0, 0, 0} = alcove:prctl(
        Drv,
        [Child],
        pr_get_name,
        <<0:(17 * 8)>>,
        0,
        0,
        0
    ),

    % set parent death signal
    %  arg2 = signal
    {ok, 0, 9, 0, 0, 0} = alcove:prctl(Drv, [Child], pr_set_pdeathsig, 9, 0, 0, 0),

    % get parent death signal
    %  arg2 = int *
    {ok, 0, <<9, 0, 0, 0>>, 0, 0, 0} = alcove:prctl(
        Drv,
        [Child],
        pr_get_pdeathsig,
        <<0:32>>,
        0,
        0,
        0
    ).

fcntl(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    Fdctl = 5,

    {ok, Flags0} = alcove:fcntl(Drv, [Child], Fdctl, f_getfd, 0),
    FD_CLOEXEC = alcove:fcntl_constant(Drv, [Child], fd_cloexec),
    Flags1 = Flags0 band (bnot FD_CLOEXEC),

    {ok, _} = alcove:fcntl(Drv, [Child], Fdctl, f_setfd, Flags1),
    {ok, Flags1} = alcove:fcntl(Drv, [Child], Fdctl, f_getfd, 0),

    {ok, _} = alcove:fcntl(Drv, [Child], Fdctl, f_setfd, Flags0),
    {ok, Flags0} = alcove:fcntl(Drv, [Child], Fdctl, f_getfd, 0).

priority(Config) ->
    Drv = ?config(drv, Config),

    {ok, Fork0} = alcove:fork(Drv, []),
    {ok, Fork1} = alcove:fork(Drv, [Fork0]),
    {ok, Fork2} = alcove:fork(Drv, [Fork0]),
    {ok, Fork3} = alcove:fork(Drv, [Fork0]),

    {ok, 0} = alcove:getpriority(Drv, [Fork0, Fork1], prio_process, 0),
    ok = alcove:setpriority(Drv, [Fork0, Fork1], prio_process, 0, 10),
    {ok, 10} = alcove:getpriority(Drv, [Fork0, Fork1], prio_process, 0),

    case alcove:getrlimit(Drv, [Fork0, Fork1], rlimit_nice) of
        {error, enotsup} ->
            ok;
        {ok, #alcove_rlimit{cur = Cur}} when Cur =:= 0 ->
            ok;
        {ok, #alcove_rlimit{}} ->
            alcove:setrlimit(
                Drv,
                [Fork0, Fork1],
                rlimit_nice,
                #alcove_rlimit{cur = 0, max = 0}
            )
    end,

    ok = alcove:setuid(Drv, [Fork0, Fork1], 65534),
    {error, eacces} = alcove:setpriority(Drv, [Fork0, Fork1], prio_process, 0, 1),

    ok = alcove:setpriority(Drv, [Fork0, Fork2], prio_process, 0, -1),
    {ok, -1} = alcove:getpriority(Drv, [Fork0, Fork2], prio_process, 0),

    {ok, Fork3} = alcove:setsid(Drv, [Fork0, Fork3]),
    {ok, 0} = alcove:getpriority(Drv, [Fork0, Fork3], prio_pgrp, 0),
    ok = alcove:setpriority(Drv, [Fork0, Fork3], prio_pgrp, 0, 10),
    {ok, 10} = alcove:getpriority(Drv, [Fork0, Fork3], prio_pgrp, 0),

    ok = alcove:exit(Drv, [Fork0], 0).

execve(Config) ->
    Drv = ?config(drv, Config),

    {ok, Child0} = alcove:fork(Drv, []),
    {ok, Child1} = alcove:fork(Drv, []),

    {'EXIT', {badarg, _}} =
        (catch alcove:execve(
            Drv,
            [Child0],
            "/usr/bin/env",
            ["/usr/bin/env"],
            ["A=1", "B=2", "C=3", false]
        )),
    ok = alcove:execve(
        Drv,
        [Child0],
        "/usr/bin/env",
        ["/usr/bin/env"],
        ["FOO=bar", "BAR=1234567"]
    ),
    ok = alcove:execve(
        Drv,
        [Child1],
        "/usr/bin/env",
        ["/usr/bin/env"],
        []
    ),

    [<<"FOO=bar\nBAR=1234567\n">>] = alcove:stdout(Drv, [Child0], 5000),
    [] = alcove:stdout(Drv, [Child1], 2000).

execvp_with_signal(Config) ->
    Drv = ?config(drv, Config),

    % Linux: cannot be PID 1 in a namespace. PID 1 can't be killed.
    {ok, Child} = alcove:fork(Drv, []),

    ok = alcove:execvp(
        Drv,
        [Child],
        "/bin/sh",
        ["/bin/sh", "-c", "kill -9 $$"]
    ),
    {termsig, sigkill} = alcove:event(Drv, [Child], 5000).

stream(Config) ->
    Drv = ?config(drv, Config),

    Chain = chain(Drv, 16),
    {ok, _} = alcove:sigaction(Drv, Chain, sigpipe, sig_dfl),
    DefaultCount = 1 * 1024 * 1024,
    Count = getenv("ALCOVE_TEST_STREAM_COUNT", integer_to_list(DefaultCount)),
    Sleep = getenv("ALCOVE_TEST_STREAM_MAGIC_SLEEP", "0"),
    % XXX procs in the fork path may exit before all the data has
    % XXX been written
    Cmd = ["yes | head -", Count, ";sleep ", Sleep],
    ok = alcove:execvp(Drv, Chain, "/bin/sh", ["/bin/sh", "-c", Cmd]),
    % <<"y\n">>
    ok = stream_count(Drv, Chain, list_to_integer(Count) * 2).

open(Config) ->
    Drv = ?config(drv, Config),

    O_RDONLY = alcove:define(Drv, [], o_rdonly),

    File = "/nonexistent",

    {error, enoent} = alcove:open(Drv, [], File, O_RDONLY, 0),
    {error, enoent} = alcove:open(Drv, [], File, [O_RDONLY, O_RDONLY, O_RDONLY, O_RDONLY], 0),
    {error, enoent} = alcove:open(Drv, [], File, [o_rdonly, o_rdonly, o_rdonly], 0).

socket(Config) ->
    Drv = ?config(drv, Config),

    {ok, Socket} = alcove:socket(Drv, [], af_inet, sock_stream, 0),
    ok = alcove:close(Drv, [], Socket).

select(Config) ->
    Drv = ?config(drv, Config),
    OS = ?config(os, Config),

    {ok, FD} = alcove:open(Drv, [], "/dev/null", [o_rdwr], 0),
    Reply = alcove:select(Drv, [], [FD], [FD], [FD], []),
    Reply = alcove:select(Drv, [], [FD], [FD], [FD], #alcove_timeval{sec = 1, usec = 1}),

    Reply =
        case OS of
            N when N =:= sunos; N =:= darwin ->
                % select(3c): File descriptors associated with regular files
                % always select true for ready to read, ready to write, and
                % error conditions.
                {ok, [FD], [FD], [FD]};
            _ ->
                {ok, [FD], [FD], []}
        end.

connect(Config) ->
    Drv = ?config(drv, Config),

    {ok, NC} = alcove:fork(Drv, []),
    {ok, Process} = alcove:fork(Drv, []),

    Sockname = <<"/tmp/alcove.", (integer_to_binary(alcove:getpid(Drv, [])))/binary>>,
    ok = alcove:execvp(Drv, [NC], "nc", ["nc", "-l", "-U", Sockname]),

    ok = waitfor(Sockname),

    {ok, Socket} = alcove:socket(Drv, [Process], af_unix, sock_stream, 0),

    % #define UNIX_PATH_MAX   108
    % struct sockaddr_un {
    % 	__kernel_sa_family_t sun_family; /* AF_UNIX */
    % 	char sun_path[UNIX_PATH_MAX];   /* pathname */
    % };
    AF_UNIX = 1,
    SocknameLen = byte_size(Sockname),
    Len = (unix_path_max() - SocknameLen) * 8,
    ok = alcove:connect(Drv, [Process], Socket, [
        sockaddr_common(AF_UNIX, SocknameLen),
        Sockname,
        <<0:Len>>
    ]),

    % alcove process -> nc
    {ok, 11} = alcove:write(Drv, [Process], Socket, <<"alcove->nc\n">>),
    <<"alcove->nc\n">> =
        receive
            {alcove_stdout, Drv, [NC], Data} ->
                Data
        end,

    % nc -> alcove process
    ok = alcove:stdin(Drv, [NC], <<"nc->alcove\n">>),
    {ok, [Socket], [], []} = alcove:select(Drv, [Process], [Socket], [], [Socket], []),
    {ok, <<"nc->alcove\n">>} = alcove:read(Drv, [Process], Socket, 11),

    ok.

% UNIX_PATH_MAX
unix_path_max() ->
    case erlang:system_info(os_type) of
        {unix, BSD} when BSD == darwin; BSD == openbsd; BSD == netbsd; BSD == freebsd ->
            104;
        {unix, _} ->
            108
    end.

% struct sockaddr
sockaddr_common(Family, Length) ->
    case erlang:system_info(os_type) of
        {unix, BSD} when BSD == darwin; BSD == openbsd; BSD == netbsd; BSD == freebsd ->
            <<Length:8, Family:8>>;
        {unix, _} ->
            <<Family:16/native>>
    end.

waitfor(Sockname) ->
    case file:read_file_info(Sockname) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            timer:sleep(1),
            waitfor(Sockname);
        {error, eperm} ->
            ok;
        Error ->
            Error
    end.

mkfifo(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    Fifo = "/tmp/alcove-fifo." ++ os:getpid(),

    ok = alcove:mkfifo(Drv, [Child], Fifo, 8#700),

    {ok, RFD} = alcove:open(Drv, [Child], Fifo, [o_rdonly, o_nonblock], 0),
    {ok, WFD} = alcove:open(Drv, [], Fifo, [o_wronly, o_nonblock], 0),

    {ok, N} = alcove:write(Drv, [], WFD, <<"123456789">>),
    {ok, <<"123456789">>} = alcove:read(Drv, [Child], RFD, N),

    ok = alcove:unlink(Drv, [Child], Fifo),

    ok.

ioctl(Config) ->
    Drv = ?config(drv, Config),
    OS = ?config(os, Config),
    NS = ?config(namespace, Config),

    case {OS, NS} of
        {linux, true} ->
            % Create a tap interface within a net namespace
            {ok, Child} = alcove:clone(Drv, [], [
                clone_newns,
                clone_newpid,
                clone_newipc,
                clone_newuts,
                clone_newnet
            ]),
            {ok, FD} = alcove:open(Drv, [Child], "/dev/net/tun", [o_rdwr], 0),
            {ok, 0, <<"tap", _/binary>>} = alcove:ioctl(Drv, [Child], FD, tunsetiff, <<
                % generate a tuntap device name
                0:(16 * 8),
                % IFF_TAP, IFF_NO_PI
                (16#0002 bor 16#1000):2/native-unsigned-integer-unit:8,
                0:(14 * 8)
            >>);
        _ ->
            {skip, "net namespaces not supported on this platform"}
    end.

symlink(Config) ->
    Drv = ?config(drv, Config),

    [Oldpath] = alcove_drv:getopts([]),
    Newpath0 = Oldpath ++ "-symlink",
    Newpath1 = Oldpath ++ "-link",

    ok = alcove:symlink(Drv, [], Oldpath, Newpath0),
    ok = alcove:link(Drv, [], Newpath0, Newpath1),

    ok = alcove:unlink(Drv, [], Newpath0),
    ok = alcove:unlink(Drv, [], Newpath1).

execvp_mid_chain(Config) ->
    Drv = ?config(drv, Config),

    Chain = chain(Drv, 8),
    {Pids, Rest} = lists:split(3, Chain),
    ok = alcove:execvp(Drv, Pids, "/bin/cat", ["/bin/cat"]),

    alcove:stdin(Drv, Pids, "test\n"),
    [<<"test\n">>] = alcove:stdout(Drv, Pids, 5000),
    false = alcove:event(Drv, Chain, 2000),
    Reply = [alcove:kill(Drv, [], Pid, 0) || Pid <- Rest],

    % The child spawned by the exec'ed process becomes a zombie
    % because the PID will not be reaped.
    [
        ok,
        {error, esrch},
        {error, esrch},
        {error, esrch},
        {error, esrch}
    ] = Reply.

execvp(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    ok = alcove:execvp(Drv, [Child], "/bin/sh", ["/bin/sh"]).

stdout(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:execvp(Drv, [Child], "/bin/sh", ["/bin/sh"]),

    ok = alcove:stdin(Drv, [Child], "echo 0123456789\n"),
    [<<"0123456789\n">>] = alcove:stdout(Drv, [Child], 5000).

stderr(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    OS = ?config(os, Config),

    ok = alcove:execvp(Drv, [Child], "/bin/sh", ["/bin/sh"]),

    ok = alcove:stdin(Drv, [Child], "nonexistent 0123456789\n"),
    Reply = alcove:stderr(Drv, [Child], 5000),

    case OS of
        N when N =:= linux; N =:= openbsd; N =:= freebsd; N =:= darwin ->
            <<"/bin/sh: ", _/binary>> = iolist_to_binary(Reply);
        netbsd ->
            <<"nonexistent: not found\n">> = iolist_to_binary(Reply);
        _ ->
            {skip, "stderr test not supported on this platform"}
    end.

pipe_buf(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:execvp(Drv, [Child], "sleep", ["sleep", "60"]),
    Stdin = binary:copy(<<"x">>, 10000),

    % Fill up the pipe buffer. On Linux, the capacity is 65535 bytes.
    [ok = alcove:stdin(Drv, [Child], Stdin) || _ <- lists:seq(1, 7)],

    Reply =
        receive
            {alcove_pipe, Drv, [Child], Bytes} ->
                {ok, Bytes}
        after 10000 -> timeout
        end,

    ok = alcove:stdin(Drv, [Child], Stdin),
    % XXX allow time for the message to be received
    timer:sleep(1000),
    {'EXIT', {{eagain, 0}, _}} = (catch alcove:stdout(Drv, [Child])),

    ok = alcove:stdin(Drv, [Child], Stdin),
    % XXX allow time for the message to be received
    timer:sleep(1000),
    {'EXIT', {{eagain, 0}, _}} = (catch alcove:stderr(Drv, [Child])),

    alcove:kill(Drv, [], Child, 9),

    {ok, _} = Reply.

signaloneof(Config) ->
    Drv = ?config(drv, Config),
    {ok, Child} = alcove:fork(Drv, []),

    true = alcove:setopt(Drv, [Child], signaloneof, 15),

    % Drv---Child-+-F0-+-F00-+-sleep (F000)
    %             |-sleep (F1)
    %             |-sleep (F2)
    {ok, F0} = alcove:fork(Drv, [Child]),
    {ok, F00} = alcove:fork(Drv, [Child, F0]),
    {ok, F000} = alcove:fork(Drv, [Child, F0, F00]),
    {ok, F1} = alcove:fork(Drv, [Child]),
    {ok, F2} = alcove:fork(Drv, [Child]),

    ok = alcove:execvp(Drv, [Child, F0, F00, F000], "sleep", ["sleep-signaloneof1", "60"]),
    ok = alcove:execvp(Drv, [Child, F1], "sleep", ["sleep-signaloneof2", "60"]),
    ok = alcove:execvp(Drv, [Child, F2], "sleep", ["sleep-signaloneof3", "60"]),

    % Force the child to exit by closing stdin
    [
        ok = alcove:close(Drv, [], Pid#alcove_pid.stdin)
     || Pid <- alcove:cpid(Drv, [])
    ],

    ok =
        receive
            {alcove_event, Drv, [Child], {exit_status, _}} ->
                ok
        after 30000 -> timeout
        end,

    % XXX allow time for processes to exit
    timer:sleep(1000),

    {error, esrch} = alcove:kill(Drv, [], F000, 0),
    {error, esrch} = alcove:kill(Drv, [], F1, 0),
    {error, esrch} = alcove:kill(Drv, [], F2, 0),

    ok.

flowcontrol(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    true = alcove:setcpid(Drv, [], Child, flowcontrol, 0),
    0 = alcove:getcpid(Drv, [], Child, flowcontrol),

    % make a call to check control processes are not flow controlled
    _ = alcove:cpid(Drv, [Child]),

    ok = alcove:execvp(Drv, [Child], "yes", ["yes-flowcontrol"]),

    % no stdout until explicitly polled
    ok = flowcontrol_wait(Drv, [Child], 3000, 0),

    true = alcove:setcpid(Drv, [], Child, flowcontrol, 2),

    ok = flowcontrol_wait(Drv, [Child], 3000, 2),

    % flowcontrol reset to 0
    0 = alcove:getcpid(Drv, [], Child, flowcontrol),

    ok.

flowcontrol_wait(Drv, Chain, Timeout, 0) ->
    receive
        {alcove_stdout, Drv, Chain, _} = Failed ->
            Failed
    after Timeout -> ok
    end;
flowcontrol_wait(Drv, Chain, Timeout, N) ->
    receive
        {alcove_stdout, Drv, Chain, _} ->
            flowcontrol_wait(Drv, Chain, Timeout, N - 1)
    after Timeout -> timeout
    end.

flowcontrol_fork_exec_exit(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    % enable default flowcontrol
    true = alcove:setopt(Drv, [Child], flowcontrol, 1),

    {ok, Proc0} = alcove:fork(Drv, [Child]),
    ok = alcove:execvp(Drv, [Child, Proc0], "echo", ["echo", "test"]),

    ok =
        receive
            {alcove_event, Drv, [Child, Proc0], {exit_status, 0}} ->
                ok
        after 5000 -> {error, timeout}
        end,

    {ok, Proc1} = alcove:fork(Drv, [Child]),
    ok = alcove:execvp(Drv, [Child, Proc1], "echo", ["echo", "test"]),

    ok =
        receive
            {alcove_event, Drv, [Child, Proc1], {exit_status, 0}} ->
                ok
        after 5000 -> {error, timeout}
        end,

    [] = alcove:cpid(Drv, [Child]),

    ok.

filter(Config) ->
    Drv = ?config(drv, Config),

    {ok, Task1} = alcove:fork(Drv, []),

    ok = alcove:filter(Drv, [], alcove_proto:call(fork)),
    ok = alcove:filter(Drv, [Task1], alcove_proto:call(getpid)),

    {ok, _} = alcove:fork(Drv, [Task1]),
    {'EXIT', {undef, _}} = (catch alcove:fork(Drv, [])),

    {'EXIT', {undef, _}} = (catch alcove:getpid(Drv, [Task1])),
    _ = alcove:getpid(Drv, []),

    NR = length(alcove_proto:calls()),

    ok = alcove:filter(Drv, [Task1], NR - 1),
    {error, einval} = alcove:filter(Drv, [Task1], NR),
    {error, einval} = alcove:filter(Drv, [Task1], NR + 1),
    {error, einval} = alcove:filter(Drv, [Task1], 16#fffffffe),

    ok.

%%
%% Portability
%%
no_os_specific_tests(_Config) ->
    {skip, "No OS specific tests defined"}.

fexecve(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    {ok, FD} = alcove:open(Drv, [Child], "/usr/bin/env", [o_rdonly, o_cloexec], 0),
    ok = alcove:fexecve(Drv, [Child], FD, [""], ["FOO=bar", "BAR=1234567"]),
    [<<"FOO=bar\nBAR=1234567\n">>] = alcove:stdout(Drv, [Child], 5000).

fexecve_sigchld(_Config) ->
    {ok, Drv} = alcove_drv:start_link(),

    % Re-exec the alcove process
    ok = fexecve_process_image(Drv, []),

    {ok, Proc0} = alcove:fork(Drv, []),
    {ok, Proc1} = alcove:fork(Drv, [Proc0]),
    ok = alcove:execvp(Drv, [Proc0, Proc1], "cat", ["cat"]),
    ok = alcove:exit(Drv, [Proc0], 0),

    ok =
        receive
            {alcove_event, Drv, [Proc0], {exit_status, _}} ->
                ok
        after 5000 -> timeout
        end,

    [] = alcove:cpid(Drv, []).

%%
%% Linux
%%

clone_constant(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    NS = ?config(namespace, Config),

    case NS of
        true ->
            CLONE_NEWNS = alcove:clone_constant(Drv, [Child], clone_newns),
            true = is_integer(CLONE_NEWNS);
        false ->
            {skip, "clone(2) not supported"}
    end.

setns(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    NS = ?config(namespace, Config),

    case NS of
        true ->
            {ok, Child1} = alcove:fork(Drv, []),
            {ok, FD} = alcove:open(
                Drv,
                [Child1],
                [
                    "/proc/",
                    integer_to_list(Child),
                    "/ns/uts"
                ],
                [o_rdonly],
                0
            ),
            ok = alcove:setns(Drv, [Child1], FD, 0),
            ok = alcove:close(Drv, [Child1], FD),

            Hostname = alcove:gethostname(Drv, [Child]),
            Hostname = alcove:gethostname(Drv, [Child1]);
        false ->
            {skip, "setns(2) not supported"}
    end.

unshare(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    NS = ?config(namespace, Config),

    case NS of
        true ->
            Host = alcove:gethostname(Drv, []),
            ok = alcove:unshare(Drv, [Child], [clone_newuts]),
            ok = alcove:sethostname(Drv, [Child], "unshare"),
            {ok, <<"unshare">>} = alcove:gethostname(Drv, [Child]),
            Host = alcove:gethostname(Drv, []);
        false ->
            {skip, "unshare(2) not supported"}
    end.

ptrace(Config) ->
    Drv = ?config(drv, Config),
    Child1 = ?config(child, Config),

    {ok, Child2} = alcove:fork(Drv, [Child1]),

    % disable the alcove event loop: child process must be managed by
    % the caller
    {ok, sig_dfl} = alcove:sigaction(Drv, [Child1], sigchld, sig_info),

    % enable ptracing in the child process and exec() a command
    {ok, 0, <<>>, <<>>} = alcove:ptrace(
        Drv,
        [Child1, Child2],
        ptrace_traceme,
        0,
        0,
        0
    ),
    ok = alcove:execvp(Drv, [Child1, Child2], "cat", ["cat"]),

    % the parent is notified
    {signal, sigchld, _} = alcove:event(Drv, [Child1], 5000),
    {ok, Child2, _, [{stopsig, sigtrap}]} = alcove:waitpid(
        Drv,
        [Child1],
        -1,
        [wnohang]
    ),

    % should be no other events
    {ok, 0, 0, []} = alcove:waitpid(Drv, [Child1], -1, [wnohang]),

    % allow the process to continue
    {ok, 0, <<>>, <<>>} = alcove:ptrace(Drv, [Child1], ptrace_cont, Child2, 0, 0),

    ok = alcove:stdin(Drv, [Child1, Child2], "test\n"),

    ok =
        receive
            {alcove_stdout, Drv, [Child1, Child2], <<"test\n">>} ->
                ok
        after 5000 -> timeout
        end,

    % Send a SIGTERM and re-write it to a harmless SIGWINCH
    ok = alcove:kill(Drv, [Child1], Child2, sigterm),
    {signal, sigchld, _} = alcove:event(Drv, [Child1], 5000),
    {ok, Child2, _, [{stopsig, sigterm}]} = alcove:waitpid(
        Drv,
        [Child1],
        -1,
        [wnohang]
    ),

    {ok, 0, <<>>, <<>>} = alcove:ptrace(
        Drv,
        [Child1],
        ptrace_cont,
        Child2,
        0,
        28
    ),

    % Convert a SIGWINCH to SIGTERM
    ok = alcove:kill(Drv, [Child1], Child2, sigwinch),
    {signal, sigchld, _} = alcove:event(Drv, [Child1], 5000),
    {ok, 0, <<>>, <<>>} = alcove:ptrace(
        Drv,
        [Child1],
        ptrace_cont,
        Child2,
        0,
        15
    ),
    {ok, Child2, _, [{termsig, sigterm}]} = alcove:waitpid(
        Drv,
        [Child1],
        -1,
        []
    ).

%%
%% FreeBSD
%%

jail(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    Jail0 = alcove_cstruct:jail(#alcove_jail{
        path = "/rescue",
        hostname = "test0",
        jailname = "jail0"
    }),
    {ok, JID0} = alcove:jail(Drv, [Child], Jail0),

    {ok, Child0} = alcove:fork(Drv, []),
    ok = alcove:jail_attach(Drv, [Child0], JID0),
    alcove:exit(Drv, [Child0], 0),

    {ok, Child1} = alcove:fork(Drv, []),
    Jail1 = alcove_cstruct:jail(#alcove_jail{
        path = "/rescue",
        hostname = "test1",
        jailname = "jail1"
    }),
    {ok, JID1} = alcove:jail(Drv, [Child1], Jail1),
    ok = alcove:jail_remove(Drv, [], JID1),
    {error, einval} = alcove:jail_attach(Drv, [], JID1).

cap_enter(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:cap_enter(Drv, [Child]),
    {ok, 1} = alcove:cap_getmode(Drv, [Child]).

cap_rights_limit(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    {ok, FD} = alcove:open(Drv, [Child], "/etc/passwd", [o_rdonly], 0),
    ok = alcove:cap_enter(Drv, [Child]),
    ok = alcove:cap_rights_limit(Drv, [Child], FD, [cap_read]),
    {ok, _} = alcove:read(Drv, [Child], FD, 64),
    {error, enotcapable} = alcove:lseek(Drv, [Child], FD, 0, 0),
    {error, ecapmode} = alcove:open(
        Drv,
        [Child],
        "/etc/passwd",
        [o_rdonly],
        0
    ).

cap_fcntls_limit(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    {ok, FD} = alcove:open(Drv, [Child], "/etc/passwd", [o_rdonly], 0),
    ok = alcove:cap_enter(Drv, [Child]),
    ok = alcove:cap_rights_limit(Drv, [Child], FD, [cap_fcntl]),
    ok = alcove:cap_fcntls_limit(Drv, [Child], FD, [cap_fcntl_setfl]),
    {error, enotcapable} = alcove:fcntl(Drv, [Child], FD, f_getfl, 0),
    Flags = alcove:cap_constant(Drv, [Child], cap_fcntl_setfl),
    {ok, Flags} = alcove:cap_fcntls_get(Drv, [Child], FD),
    {ok, 0} = alcove:fcntl(Drv, [Child], FD, f_setfl, 0).

cap_ioctls_limit(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    % port process does not have a tty: try to find a pty
    % owned by the user running the test
    {ok, FD} = open_pty(Drv, [Child]),
    ok = alcove:cap_enter(Drv, [Child]),
    ok = alcove:cap_rights_limit(Drv, [Child], FD, [cap_ioctl]),
    ok = alcove:cap_ioctls_limit(Drv, [Child], FD, [tiocmget, tiocgwinsz]),
    {error, enotcapable} = alcove:ioctl(Drv, [Child], FD, tiocmset, <<>>),
    {ok, 0, <<>>} = alcove:ioctl(Drv, [Child], FD, tiocmget, <<>>),
    {ok, 0, <<>>} = alcove:ioctl(Drv, [Child], FD, tiocgwinsz, <<>>).

open_pty(Drv, Child) ->
    {ok, Ptys} = alcove:readdir(Drv, Child, "/dev/pts"),
    open_pty(
        Drv,
        Child,
        lists:filter(fun(N) -> N =/= <<".">> andalso N =/= <<"..">> end, Ptys)
    ).

open_pty(_Drv, _Child, []) ->
    {error, enxio};
open_pty(Drv, Child, [Pty | Ptys]) ->
    case alcove:open(Drv, Child, ["/dev/pts/", Pty], [o_rdwr, o_nonblock], 0) of
        {ok, FD} ->
            {ok, FD};
        {error, _} ->
            open_pty(Drv, Child, Ptys)
    end.

%%
%% OpenBSD
%%
pledge(Config) ->
    Drv = ?config(drv, Config),

    ok = alcove:setrlimit(Drv, [], rlimit_core, #alcove_rlimit{
        cur = 0,
        max = 0
    }),

    {ok, Fork0} = alcove:fork(Drv, []),
    {ok, Fork1} = alcove:fork(Drv, []),

    {ok, Fork2} = alcove:fork(Drv, []),
    {ok, Fork3} = alcove:fork(Drv, []),

    % NULL does modify the pledge list
    ok = alcove:pledge(Drv, [Fork0], null, ""),
    ok = alcove:pledge(Drv, [Fork0], 'NULL', ""),

    ok = alcove:pledge(Drv, [Fork0], "stdio", ""),
    ok = alcove:pledge(Drv, [Fork1], "stdio proc exec", ""),

    ok = alcove:pledge(Drv, [Fork2], "stdio exec", null),
    ok = alcove:pledge(Drv, [Fork3], "stdio exec", ""),

    alcove:getuid(Drv, [Fork0]),
    alcove:getuid(Drv, [Fork1]),

    {'EXIT', {{termsig, sigabrt}, _}} = (catch alcove:fork(Drv, [Fork0])),
    {ok, _} = alcove:fork(Drv, [Fork1]),
    {'EXIT', {{termsig, sigabrt}, _}} =
        (catch alcove:open(Drv, [Fork1], "/etc/passwd", [o_rdonly], 0)),

    ok = alcove:execvp(Drv, [Fork2], "cat", ["shouldrun"]),

    alcove:stdin(Drv, [Fork2], "test\n"),
    [<<"test\n">>] = alcove:stdout(Drv, [Fork2], 5000),
    false = alcove:event(Drv, [Fork2], 2000),

    %{'EXIT',{{termsig,sigabrt},_}} = (catch alcove:execvp(Drv, [Fork3], "cat", ["shouldfail"])),
    ok = (catch alcove:execvp(Drv, [Fork3], "cat", ["shouldfail"])),

    {termsig, sigabrt} = alcove:event(Drv, [Fork3], 2000),

    ok.

unveil(Config) ->
    Drv = ?config(drv, Config),

    {ok, Proc} = alcove:fork(Drv, []),

    ok = alcove:unveil(Drv, [Proc], "/etc", "r"),
    ok = alcove:unveil(Drv, [Proc], "/bin", "rx"),
    ok = alcove:unveil(Drv, [Proc], "/sbin", "r"),
    ok = alcove:unveil(Drv, [Proc], null, null),

    {ok, SubProc} = alcove:fork(Drv, [Proc]),

    % /usr/bin: not unveiled
    {error, enoent} = alcove:open(Drv, [Proc, SubProc], "/usr/bin/vi", [o_rdonly], 0),

    % /etc: unveiled
    {ok, FD1} = alcove:open(Drv, [Proc, SubProc], "/etc/passwd", [o_rdonly], 0),
    {ok, _} = alcove:read(Drv, [Proc, SubProc], FD1, 1024),

    % /sbin: unveiled: read only
    {ok, FD2} = alcove:open(Drv, [Proc, SubProc], "/sbin/mkfifo", [o_rdonly], 0),
    {ok, _} = alcove:read(Drv, [Proc, SubProc], FD2, 1024),

    {error, eacces} = alcove:execvp(Drv, [Proc, SubProc], "/sbin/mkfifo", ["mkfifo", "-h"]),

    % /bin: unveiled: read + execute
    ok = alcove:execvp(Drv, [Proc, SubProc], "/bin/ls", ["ls", "-h"]),

    ok.

process_tree_leader_exits(Config) ->
    Drv = ?config(drv, Config),
    {ok, Child} = alcove:fork(Drv, []),
    Pid = alcove:getpid(Drv, [Child]),
    ok = alcove:execvp(Drv, [Child], "/bin/sh", ["sh", "-c", "sleep 300"]),
    ok = alcove:kill(Drv, [], Pid, 15),
    _ = alcove:getpid(Drv, []),
    _ = alcove:getpid(Drv, []),
    _ = alcove:getpid(Drv, []),
    ok.

%%
%% Utility functions
%%
getenv(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Env -> Env
    end.

get_unused_pid(Drv) ->
    get_unused_pid(Drv, 16#0affffff).

get_unused_pid(Drv, PID) ->
    case alcove:kill(Drv, [], PID, 0) of
        {error, esrch} -> PID;
        _ -> get_unused_pid(Drv, PID + 1)
    end.

forkstress_1(_Drv, _Child, 0) ->
    ok;
forkstress_1(Drv, Child, N) ->
    {ok, Fork} = alcove:fork(Drv, [Child]),
    ok = alcove:exit(Drv, [Child, Fork], 0),
    Version = alcove:version(Drv, [Child]),
    Version = alcove:version(Drv, [Child]),
    forkstress_1(Drv, Child, N - 1).

chain(Drv, N) ->
    chain(Drv, [], N).

chain(_Drv, Fork, 0) ->
    Fork;
chain(Drv, Fork, N) ->
    {ok, Child} = alcove:fork(Drv, Fork),
    chain(Drv, Fork ++ [Child], N - 1).

stream_count(_Drv, _Chain, 0) ->
    ok;
stream_count(Drv, Chain, N) ->
    receive
        {alcove_stdout, Drv, Chain, Bin} ->
            stream_count(Drv, Chain, N - byte_size(Bin))
    after 1000 -> {error, N}
    end.

%
% re-exec the alcove program
%
fexecve_process_image(Drv, Chain) ->
    Progname = alcove_drv:progname(),
    {ok, FD} = alcove:open(Drv, Chain, Progname, [o_rdonly, o_cloexec], 0),
    Argv = alcove_drv:getopts([
        {progname, Progname},
        {depth, length(Chain)}
    ]),
    Env = alcove:environ(Drv, Chain),

    ok = setflag(Drv, Chain, [3, 4, 5, FD], fd_cloexec, unset),
    Reply = alcove:fexecve(Drv, Chain, FD, Argv, Env),
    ok = setflag(Drv, Chain, [3, 4, 5, FD], fd_cloexec, set),
    Reply.

setflag(_Drv, _Chain, [], _Flag, _Status) ->
    ok;
setflag(Drv, Chain, [FD | FDSet], Flag, Status) ->
    Constant = alcove:fcntl_constant(Drv, Chain, Flag),
    case alcove:fcntl(Drv, Chain, FD, f_getfd, 0) of
        {ok, Flags} ->
            case
                alcove:fcntl(
                    Drv,
                    Chain,
                    FD,
                    f_setfd,
                    fdstatus(Flags, Constant, Status)
                )
            of
                {ok, _NewFlags} ->
                    setflag(Drv, Chain, FDSet, Flag, Status);
                Error1 ->
                    Error1
            end;
        Error ->
            Error
    end.

fdstatus(Flags, Constant, set) -> Flags bor Constant;
fdstatus(Flags, Constant, unset) -> Flags band (bnot Constant).
