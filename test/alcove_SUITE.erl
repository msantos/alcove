%%% Copyright (c) 2014-2015, Michael Santos <michael.santos@gmail.com>
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
        chroot/1,
        clone_constant/1,
        env/1,
        eof/1,
        event/1,
        execve/1,
        execvp/1,
        execvp_mid_chain/1,
        execvp_with_signal/1,
        fcntl/1,
        fexecve/1,
        fork/1,
        forkchain/1,
        forkstress/1,
        getpid/1,
        ioctl/1,
        iodata/1,
        jail/1,
        mount/1,
        mount_constant/1,
        open/1,
        pid/1,
        portstress/1,
        prctl/1,
        priority/1,
        ptrace/1,
        select/1,
        setgid/1,
        setgroups/1,
        sethostname/1,
        setns/1,
        setopt/1,
        setrlimit/1,
        setuid/1,
        signal/1,
        stderr/1,
        stdout/1,
        stream/1,
        symlink/1,
        tmpfs/1,
        unshare/1,
        version/1,

        no_os_specific_tests/1
    ]).

all() ->
    {unix, OS} = os:type(),
    [
        {group, OS},
        version,
        iodata,
        pid,
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
        setgroups,
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
        select,
        ioctl,
        symlink,
        execvp_mid_chain
    ].

groups() ->
    [
        {linux, [sequence], [
                fexecve,
                clone_constant,
                setns,
                unshare,
                prctl,
                ptrace
            ]},
        {freebsd, [sequence], [
                fexecve,
                jail,
                cap_enter,
                cap_rights_limit,
                cap_fcntls_limit,
                cap_ioctls_limit
            ]},
        {darwin, [], [no_os_specific_tests]},
        {netbsd, [], [no_os_specific_tests]},
        {openbsd, [], [no_os_specific_tests]},
        {solaris, [], [no_os_specific_tests]}
    ].

init_per_testcase(_Test, Config) ->
    % export ALCOVE_TEST_EXEC="sudo valgrind --leak-check=yes --log-file=/tmp/alcove.log"
    Exec = getenv("ALCOVE_TEST_EXEC", "sudo"),
    Use_fork = false =/= getenv("ALCOVE_TEST_USE_FORK", false),

    {ok, Drv} = alcove_drv:start_link([{exec, Exec}, {maxchild, 8}]),

    case {Use_fork, os:type()} of
        {false, {unix,linux}} ->
            {ok, Child} = alcove:clone(Drv, [], [
                    clone_newipc,
                    clone_newnet,
                    clone_newns,
                    clone_newpid,
                    clone_newuts
                ]),
            [{drv, Drv},
                {child, Child},
                {namespace, true},
                {os, linux}|Config];

        {_, {unix, OS}} ->
            {ok, Child} = alcove:fork(Drv, []),
            [{drv, Drv},
                {child, Child},
                {namespace, false},
                {os, OS}|Config]
    end.

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
    Iolist = ["0",1,"2",3,
              [4,[5,6,<<7,8,9>>,["10"]]],
              <<"1112, 13, 14, 15">>,
              [16,17,"18,19,20"],
              21,
              <<22>>],

    Bin = alcove:iolist_to_bin(Drv, [], Iolist),
    Bin = iolist_to_binary(Iolist),

    % Valid iolists: binary, string, lists, bytes must be within a list
    {'EXIT',{badarg,_}} = (catch alcove:iolist_to_bin(Drv, [], 10)),
    {'EXIT',{badarg,_}} = (catch alcove:iolist_to_bin(Drv, [], [123456])),
    <<1,2,3,4,5,6>> = alcove:iolist_to_bin(Drv, [], <<1,2,3,4,5,6>>),
    <<"ok">> = alcove:iolist_to_bin(Drv, [], "ok"),

    % Arbitrary implementation limit of 16 nested
    % lists. iolist_to_binary/1 does not have this limitation.
    <<"ok">> = alcove:iolist_to_bin(Drv, [],
        [[[[[[[[[[[[[[[["ok"]]]]]]]]]]]]]]]]),

    {'EXIT',{badarg,_}} = (catch alcove:iolist_to_bin(Drv, [],
            [[[[[[[[[[[[[[[[["fail"]]]]]]]]]]]]]]]]])),

    ok.

pid(Config) ->
    Drv = ?config(drv, Config),

    {ok, Child} = alcove:fork(Drv, []),
    {ok, Grandchild} = alcove:fork(Drv, [Child]),
    [#alcove_pid{}] = alcove:pid(Drv, [Child]),
    ok = alcove:execvp(Drv, [Child,Grandchild], "/bin/cat", ["/bin/cat"]),
    [#alcove_pid{fdctl = -2}] = alcove:pid(Drv, [Child]),
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

    true = alcove:setopt(Drv, [Fork], maxchild, 128),

    {ok, Fork1} = alcove:fork(Drv, [Fork]),

    128 = alcove:getopt(Drv, [Fork], maxchild),
    128 = alcove:getopt(Drv, [Fork, Fork1], maxchild),

    alcove:exit(Drv, [Fork, Fork1], 0),

    true = alcove:setopt(Drv, [Fork], exit_status, 0),
    true = alcove:setopt(Drv, [Fork], maxforkdepth, 0),

    0 = alcove:getopt(Drv, [Fork], exit_status),
    true = 0 =/= alcove:getopt(Drv, [], maxforkdepth),
    0 = alcove:getopt(Drv, [Fork], maxforkdepth),
    {error, eagain} = alcove:fork(Drv, [Fork]).

event(Config) ->
    Drv = ?config(drv, Config),

    {ok,_} = alcove:sigaction(Drv, [], sigchld, sig_catch),

    {ok, Fork} = alcove:fork(Drv, []),
    ok = alcove:exit(Drv, [Fork], 0),
    {ok, Fork, 0, [{exit_status, 0}]} = alcove:waitpid(Drv, [], -1, 0),
    {signal, _} = alcove:event(Drv, [], 5000).

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
            ok = alcove:mount(Drv, [Child], "/tmp", "/mnt", "", [
                    ms_bind,
                    ms_rdonly,
                    ms_noexec
                ], "", ""),
            ok = alcove:umount(Drv, [Child], "/mnt");

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
            ok = alcove:mount(Drv, [Child], "tmpfs", "/mnt", "tmpfs", [ms_noexec], <<"size=16M", 0>>, <<>>),
            ok = alcove:umount(Drv, [Child], "/mnt");

        {N, false} when N =:= linux; N =:= openbsd ->
            % Linux: running in a fork in the global namespace
            Dir = "/tmp/alcove." ++ [ crypto:rand_uniform(16#30,16#39) || _ <- lists:seq(1,8) ],
            ok = alcove:mkdir(Drv, [Child], Dir, 8#700),
            ok = alcove:mount(Drv, [Child], "tmpfs", Dir, "tmpfs", [noexec],
                <<"size=16M", 0>>, <<>>),
            ok = alcove:umount(Drv, [Child], Dir),
            ok = alcove:rmdir(Drv, [Child], Dir);

        {sunos, false} ->
            Dir = "/tmp/alcove." ++ [ crypto:rand_uniform(16#30,16#39) || _ <- lists:seq(1,8) ],
            ok = alcove:mkdir(Drv, [Child], Dir, 8#700),
            ok = alcove:mount(Drv, [Child], "swap", Dir, "tmpfs", [ms_optionstr],
                <<>>, <<"size=16m", 0:4096>>),
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

setrlimit(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:setrlimit(Drv, [Child], rlimit_nofile, #alcove_rlimit{
            cur = 64,
            max = 64
        }),
    {ok, #alcove_rlimit{cur = 64, max = 64}} = alcove:getrlimit(Drv, [Child],
        rlimit_nofile).

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

    Reply1 = case OS of
        freebsd ->
            [hd(Groups)];
        _ ->
            []
    end.

fork(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    Pids = [ alcove:fork(Drv, [Child]) || _ <- lists:seq(1, 32) ],
    [{error, eagain}|_Rest] = lists:reverse(Pids),
    Reply0 = alcove:getpid(Drv, [Child]),
    true = is_integer(Reply0),

    [{ok, Child0}|_] = [ N || N <- Pids, N =/= {error,eagain} ],
    ok = alcove:exit(Drv, [Child, Child0], 0),
    {exit_status, 0} = alcove:event(Drv, [Child, Child0], 5000),
    {ok, _} = alcove:fork(Drv, [Child]),
    {error, eagain} = alcove:fork(Drv, [Child]).

badpid(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    % EPIPE or PID not found
    ok = alcove:execvp(Drv, [Child], "/bin/sh",
        ["/bin/sh", "-c", "echo > /dev/null"]),
    {exit_status, 0} = alcove:event(Drv, [Child], 5000),
    {'EXIT',{badpid,_}} = (catch alcove:execvp(Drv, [Child],
            "/bin/sh", ["/bin/sh", "-c", "echo > /dev/null"])),

    % PID not found
    PID = get_unused_pid(Drv),
    {'EXIT',{badpid,_}} = (catch alcove:execvp(Drv, [PID],
            "/bin/sh", ["/bin/sh", "-c", "echo > /dev/null"])),

    % Invalid PIDs
    {'EXIT',{badpid,_}} = (catch alcove:execvp(Drv, [0], "/bin/sh",
            ["/bin/sh", "-c", "echo > /dev/null"])),

    ok.

signal(Config) ->
    Drv = ?config(drv, Config),

    % Linux: cannot be PID 1 in a namespace. PID 1 can't be killed.
    {ok, Child} = alcove:fork(Drv, []),

    {ok, sig_dfl} = alcove:sigaction(Drv, [Child], sigterm, sig_ign),
    ok = alcove:kill(Drv, [], Child, sigterm),
    Pid0 = alcove:getpid(Drv, [Child]),
    true = is_integer(Pid0),

    {ok, sig_ign} = alcove:sigaction(Drv, [Child], sigterm, sig_dfl),
    ok = alcove:kill(Drv, [], Child, sigterm),
    {termsig, sigterm} = alcove:event(Drv, [Child], 5000),
    alcove:kill(Drv, [], Child, 0),
    {error, esrch} = alcove:kill(Drv, [], Child, 0).

portstress(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    Versions = [ alcove:version(Drv, [Child]) || _ <- lists:seq(1,1000) ],
    Versions = lists:filter(fun
            (false) -> false;
            (_) -> true
            end, Versions).

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

    ok = alcove:eof(Drv, [Child,Child0], stderr),
    {error, ebadf} = alcove:eof(Drv, [Child,Child0], stderr),

    ok = alcove:eof(Drv, [Child,Child0], stdout),
    {error, ebadf} = alcove:eof(Drv, [Child,Child0], stdout),

    ok = alcove:eof(Drv, [Child,Child0]),
    ok = case alcove:eof(Drv, [Child,Child0]) of
        {error,esrch} -> ok;
        {error,ebadf} -> ok;
        N -> N
    end.

alloc(Config) ->
    Drv = ?config(drv, Config),

    {ok, Buf, Cstruct} = alcove:alloc(Drv, [],
        [<<1,2,3,4,5,6,7,8,9,10>>,
         {ptr, 11},
         <<11,12,13,14,15>>,
         {ptr, <<16,17,18,19,20,21,22>>},
         <<23,24,25>>]
    ),
    Size = erlang:system_info({wordsize,external}),
    Buflen = 10 + Size + 5 + Size + 3,
    Buflen = byte_size(Buf),

    <<1,2,3,4,5,6,7,8,9,10>> = lists:nth(1, Cstruct),
    {ptr, <<0:88>>} = lists:nth(2, Cstruct),
    <<11,12,13,14,15>> = lists:nth(3, Cstruct),
    {ptr, <<16,17,18,19,20,21,22>>} = lists:nth(4, Cstruct),
    <<23,24,25>> = lists:nth(5, Cstruct),

    Bits = Size * 8,

    {'EXIT',{badarg,_}} = (catch alcove:alloc(Drv, [], [])),
    {ok, <<>>, [<<>>]} = alcove:alloc(Drv, [], [<<>>]),
    {ok, <<>>, [<<>>, <<>>]} = alcove:alloc(Drv, [], [<<>>, <<>>]),
    {ok, <<>>, [<<>>, <<>>, <<>>]} = alcove:alloc(Drv, [], [<<>>, <<>>, <<>>]),
    {ok, <<0:Bits>>, [<<0:Bits>>]} = alcove:alloc(Drv, [], [{ptr, 0}]),
    {ok, <<0:Bits>>, [<<0:Bits>>]} = alcove:alloc(Drv, [], [{ptr, <<>>}]),

    {ok, BufWithNulls, CstructWithNulls} = alcove:alloc(Drv, [], [
            <<>>, {ptr, 0}, <<"foo123">>, {ptr, <<>>}, <<"bar456789">>
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
    OS = ?config(os, Config),

    case OS of
        linux ->
            % capability is set:
            %   returns 0 | 1 in function result, arg2 = int
            {ok,1,0,0,0,0} = alcove:prctl(Drv, [Child], pr_capbset_read, 0, 0,0,0),

            % set process name:
            %   arg2 = char *, up to 16 bytes, NULL terminated
            {ok,0,<<116,101,115,116,0>>,0,0,0} = alcove:prctl(Drv, [Child], pr_set_name, <<"test",0>>, 0,0,0),

            % get process name
            %   value returned in arg2 = char *, up to 16 bytes
            {ok,0,<<116,101,115,116,0,0,0,0,0,0,0,0,0,0,0,0,0>>,0,0,0} = alcove:prctl(Drv, [Child], pr_get_name, <<0:(17*8)>>, 0,0,0),

            % set parent death signal
            %  arg2 = signal
            {ok,0,9,0,0,0} = alcove:prctl(Drv, [Child], pr_set_pdeathsig, 9, 0,0,0),

            % get parent death signal
            %  arg2 = int *
            {ok,0,<<9,0,0,0>>,0,0,0} = alcove:prctl(Drv, [Child], pr_get_pdeathsig, <<0:32>>, 0,0,0);

        _ ->
            {skip, "prctl(2) not supported"}
    end.

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

    {ok, 0} = alcove:getpriority(Drv, [Fork0,Fork1], prio_process, 0),
    ok = alcove:setpriority(Drv, [Fork0,Fork1], prio_process, 0, 10),
    {ok, 10} = alcove:getpriority(Drv, [Fork0,Fork1], prio_process, 0),

    case alcove:getrlimit(Drv, [Fork0,Fork1], rlimit_nice) of
        {error,enotsup} ->
            ok;
        {ok, #alcove_rlimit{cur = Cur}} when Cur =:= 0 ->
            ok;
        {ok, #alcove_rlimit{}} ->
            alcove:setrlimit(Drv, [Fork0,Fork1], rlimit_nice,
                #alcove_rlimit{cur = 0, max = 0})
    end,

    ok = alcove:setuid(Drv, [Fork0,Fork1], 65534),
    {error, eacces} = alcove:setpriority(Drv, [Fork0,Fork1], prio_process, 0, 1),

    ok = alcove:setpriority(Drv, [Fork0,Fork2], prio_process, 0, -1),
    {ok, -1} = alcove:getpriority(Drv, [Fork0,Fork2], prio_process, 0),

    ok = alcove:exit(Drv, [Fork0], 0).

execve(Config) ->
    Drv = ?config(drv, Config),

    {ok, Child0} = alcove:fork(Drv, []),
    {ok, Child1} = alcove:fork(Drv, []),

    {'EXIT',{badarg,_}} = (catch alcove:execve(Drv, [Child0], "/usr/bin/env",
            ["/usr/bin/env"], ["A=1", "B=2", "C=3", false])),
    ok = alcove:execve(Drv, [Child0], "/usr/bin/env",
        ["/usr/bin/env"], ["FOO=bar", "BAR=1234567"]),
    ok = alcove:execve(Drv, [Child1], "/usr/bin/env",
        ["/usr/bin/env"], []),

    <<"FOO=bar\nBAR=1234567\n">> = alcove:stdout(Drv, [Child0], 5000),
    false = alcove:stdout(Drv, [Child1], 2000).

execvp_with_signal(Config) ->
    Drv = ?config(drv, Config),

    % Linux: cannot be PID 1 in a namespace. PID 1 can't be killed.
    {ok, Child} = alcove:fork(Drv, []),

    ok = alcove:execvp(Drv, [Child], "/bin/sh",
        ["/bin/sh", "-c", "kill -9 $$"]),
    {termsig, sigkill} = alcove:event(Drv, [Child], 5000).

stream(Config) ->
    Drv = ?config(drv, Config),

    Chain = chain(Drv, 16),
    {ok,_} = alcove:sigaction(Drv, Chain, sigpipe, sig_dfl),
    DefaultCount = 1 * 1024 * 1024,
    Count = getenv("ALCOVE_TEST_STREAM_COUNT", integer_to_list(DefaultCount)),
    Sleep = getenv("ALCOVE_TEST_STREAM_MAGIC_SLEEP", "0"),
    % XXX procs in the fork path may exit before all the data has
    % XXX been written
    Cmd = ["yes | head -", Count, ";sleep ", Sleep],
    ok = alcove:execvp(Drv, Chain, "/bin/sh", ["/bin/sh", "-c", Cmd]),
    % <<"y\n">>
    ok = stream_count(Drv, Chain, list_to_integer(Count)*2).

open(Config) ->
    Drv = ?config(drv, Config),

    O_RDONLY = alcove:define(Drv, [], o_rdonly),

    File = "/nonexistent",

    {error, enoent} = alcove:open(Drv, [], File, O_RDONLY, 0),
    {error, enoent} = alcove:open(Drv, [], File, [O_RDONLY,O_RDONLY,O_RDONLY,O_RDONLY], 0),
    {error, enoent} = alcove:open(Drv, [], File, [o_rdonly, o_rdonly, o_rdonly], 0).

select(Config) ->
    Drv = ?config(drv, Config),
    OS = ?config(os, Config),

    {ok, FD} = alcove:open(Drv, [], "/dev/null", [o_rdwr], 0),
    Reply = alcove:select(Drv, [], [FD], [FD], [FD], <<>>),
    Reply = alcove:select(Drv, [], [FD], [FD], [FD], #alcove_timeval{sec = 1, usec = 1}),

    Reply = case OS of
        sunos ->
            % select(3c): File descriptors associated with regular files
            % always select true for ready to read, ready to write, and
            % error conditions.
            {ok, [FD], [FD], [FD]};
        _ ->
            {ok, [FD], [FD], []}
    end.

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
            {ok,<<"tap", _/binary>>} = alcove:ioctl(Drv, [Child], FD, tunsetiff, <<
                0:(16*8),         % generate a tuntap device name
                (16#0002 bor 16#1000):2/native-unsigned-integer-unit:8, % IFF_TAP, IFF_NO_PI
                0:(14*8)
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
    OS = ?config(os, Config),

    Chain = chain(Drv, 8),
    {Pids, Rest} = lists:split(3, Chain),
    ok = alcove:execvp(Drv, Pids, "/bin/cat", ["/bin/cat"]),

    alcove:stdin(Drv, Pids, "test\n"),
    <<"test\n">> = alcove:stdout(Drv, Pids, 5000),
    false = alcove:event(Drv, Chain, 2000),
    Reply = [ alcove:kill(Drv, [], Pid, 0) || Pid <- Rest ],

    ChildState = case OS of
        openbsd -> {error,esrch};
        _ -> ok
    end,

    % The child spawned by the exec'ed process becomes a zombie
    % because the PID will not be reaped.
    [ChildState,
        {error, esrch},
        {error, esrch},
        {error, esrch},
        {error, esrch}] = Reply.

execvp(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    ok = alcove:execvp(Drv, [Child], "/bin/sh", ["/bin/sh"]).

stdout(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    ok = alcove:execvp(Drv, [Child], "/bin/sh", ["/bin/sh"]),

    true = alcove:stdin(Drv, [Child], "echo 0123456789\n"),
    <<"0123456789\n">> = alcove:stdout(Drv, [Child], 5000).

stderr(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),
    OS = ?config(os, Config),

    ok = alcove:execvp(Drv, [Child], "/bin/sh", ["/bin/sh"]),

    true = alcove:stdin(Drv, [Child], "nonexistent 0123456789\n"),
    Reply = alcove:stderr(Drv, [Child], 5000),

    case OS of
        N when N =:= linux; N =:= openbsd ->
            <<"/bin/sh: ", _/binary>> = Reply;
        N when N =:= freebsd; N =:= netbsd ->
            <<"nonexistent: not found\n">> = Reply;
        _ ->
            {skip, "stderr test not supported on this platform"}
    end.

%%
%% Portability
%%
no_os_specific_tests(_Config) ->
    {skip, "No OS specific tests defined"}.

fexecve(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    {ok, FD} = alcove:open(Drv, [Child], "/usr/bin/env", [o_rdonly,o_cloexec], 0),
    ok = alcove:fexecve(Drv, [Child], FD, [""], ["FOO=bar", "BAR=1234567"]),
    <<"FOO=bar\nBAR=1234567\n">> = alcove:stdout(Drv, [Child], 5000).

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
            {ok,FD} = alcove:open(Drv, [Child1], [
                    "/proc/",
                    integer_to_list(Child),
                    "/ns/uts"
                ], [o_rdonly], 0),
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
    {ok, sig_dfl} = alcove:sigaction(Drv, [Child1], sigchld, sig_catch),

    % enable ptracing in the child process and exec() a command
    {ok, 0, <<>>, <<>>} = alcove:ptrace(Drv, [Child1, Child2], ptrace_traceme,
        0, 0, 0),
    ok = alcove:execvp(Drv, [Child1, Child2], "cat", ["cat"]),

    % the parent is notified
    {signal, sigchld} = alcove:event(Drv, [Child1], 5000),
    {ok, Child2, _, [{stopsig, sigtrap}]} = alcove:waitpid(Drv, [Child1], -1,
        [wnohang]),

    % should be no other events
    {ok, 0, 0, []} = alcove:waitpid(Drv, [Child1], -1, [wnohang]),

    % allow the process to continue
    {ok, 0, <<>>, <<>>} = alcove:ptrace(Drv, [Child1], ptrace_cont, Child2, 0, 0),

    true = alcove:stdin(Drv, [Child1, Child2], "test\n"),

    ok = receive
        {alcove_stdout, Drv, [Child1, Child2], <<"test\n">>} ->
            ok
    after
        5000 ->
            timeout
    end,

    % Send a SIGTERM and re-write it to a harmless SIGWINCH
    ok = alcove:kill(Drv, [Child1], Child2, sigterm),
    {signal, sigchld} = alcove:event(Drv, [Child1], 5000),
    {ok, Child2, _, [{stopsig, sigterm}]} = alcove:waitpid(Drv, [Child1], -1,
        [wnohang]),

    {ok, 0, <<>>, <<>>} = alcove:ptrace(Drv, [Child1], ptrace_cont,
        Child2, 0, 28),

    % Convert a SIGWINCH to SIGTERM
    ok = alcove:kill(Drv, [Child1], Child2, sigwinch),
    {signal, sigchld} = alcove:event(Drv, [Child1], 5000),
    {ok, 0, <<>>, <<>>} = alcove:ptrace(Drv, [Child1], ptrace_cont,
        Child2, 0, 15),
    {ok, Child2, _, [{termsig, sigterm}]} = alcove:waitpid(Drv, [Child1], -1,
        [wnohang]).

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
    {error,enotcapable} = alcove:lseek(Drv, [Child], FD, 0, 0),
    {error,ecapmode} = alcove:open(Drv, [Child], "/etc/passwd",
        [o_rdonly], 0).

cap_fcntls_limit(Config) ->
    Drv = ?config(drv, Config),
    Child = ?config(child, Config),

    {ok, FD} = alcove:open(Drv, [Child], "/etc/passwd", [o_rdonly], 0),
    ok = alcove:cap_enter(Drv, [Child]),
    ok = alcove:cap_rights_limit(Drv, [Child], FD, [cap_fcntl]),
    ok = alcove:cap_fcntls_limit(Drv, [Child], FD, [cap_fcntl_setfl]),
    {error, enotcapable} = alcove:fcntl(Drv, [Child], FD, f_getfl, 0),
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
    {ok, <<>>} = alcove:ioctl(Drv, [Child], FD, tiocmget, <<>>),
    {ok, <<>>} = alcove:ioctl(Drv, [Child], FD, tiocgwinsz, <<>>).

open_pty(Drv, Child) ->
    {ok, Ptys} = alcove:readdir(Drv, Child, "/dev/pts"),
    open_pty(
      Drv,
      Child,
      lists:filter(fun(N) -> N =/= <<".">> andalso N =/= <<"..">> end, Ptys)
    ).

open_pty(_Drv, _Child, []) ->
    {error, enxio};
open_pty(Drv, Child, [Pty|Ptys]) ->
    case alcove:open(Drv, Child, ["/dev/pts/", Pty], [o_rdwr,o_nonblock], 0) of
        {ok, FD} ->
            {ok, FD};
        {error, _} ->
            open_pty(Drv, Child, Ptys)
    end.

%%
%% Utility functions
%%
getenv(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Env -> Env
    end.

get_unused_pid(Drv) ->
    PID = crypto:rand_uniform(16#0affffff, 16#0fffffff),
    case alcove:kill(Drv, [], PID, 0) of
        {error,esrch} -> PID;
        _ -> get_unused_pid(Drv)
    end.

forkstress_1(_Drv, _Child, 0) ->
    ok;
forkstress_1(Drv, Child, N) ->
    {ok, Fork} = alcove:fork(Drv, [Child]),
    ok = alcove:exit(Drv, [Child,Fork], 0),
    Version = alcove:version(Drv, [Child]),
    Version = alcove:version(Drv, [Child]),
    forkstress_1(Drv, Child, N-1).

chain(Drv, N) ->
    chain(Drv, [], N).

chain(_Drv, Fork, 0) ->
    Fork;
chain(Drv, Fork, N) ->
    {ok, Child} = alcove:fork(Drv, Fork),
    chain(Drv, Fork ++ [Child], N-1).

stream_count(_Drv, _Chain, 0) ->
    ok;
stream_count(Drv, Chain, N) ->
    receive
        {alcove_stdout, Drv, Chain, Bin} ->
            stream_count(Drv, Chain, N - byte_size(Bin))
    after
        1000 ->
            {error, N}
    end.
