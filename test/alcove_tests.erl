%%% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
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
-module(alcove_tests).

-compile(export_all).

-record(state, {
        pid,
        child,
        os,
        clone = false
    }).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("alcove/include/alcove.hrl").

alcove_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun run/1
    }.

run(State) ->
    % Test order must be maintained
    [
        msg(State),
        version(State),
        iodata(State),
        pid(State),
        getpid(State),
        setopt(State),
        event(State),
        sethostname(State),
        env(State),
        clone_define(State),
        setns(State),
        unshare(State),
        mount_define(State),
        mount(State),
        tmpfs(State),
        chroot(State),
        chdir(State),
        setrlimit(State),
        setgid(State),
        setuid(State),
        fork(State),
        badpid(State),
        signal(State),
        portstress(State),
        forkstress(State),
        forkchain(State),
        eof(State),
        alloc(State),
        prctl(State),
        priority(State),
        execvp(State),
        execvp_with_signal(State),
        stdout(State),
        stderr(State),
        execve(State),
        stream(State),
        open(State),
        execvp_mid_chain(State)
    ].

start() ->
    % export ALCOVE_TEST_EXEC="sudo valgrind --leak-check=yes --log-file=/tmp/alcove.log"
    Exec = getenv("ALCOVE_TEST_EXEC", "sudo"),
    Use_fork = false =/= getenv("ALCOVE_TEST_USE_FORK", false),

    {ok, Drv} = alcove_drv:start_link([{exec, Exec}, {maxchild, 8}]),

    ok = alcove:sigaction(Drv, sigchld, sig_catch),
    ok = alcove:sigaction(Drv, sigpipe, sig_ign),

    case {Use_fork, os:type()} of
        {false, {unix,linux} = OS} ->
            {ok, Child} = alcove:clone(Drv, [
                    clone_newipc,
                    clone_newnet,
                    clone_newns,
                    clone_newpid,
                    clone_newuts
                ]),
            #state{
                pid = Drv,
                child = Child,
                clone = true,
                os = OS
            };
        {_, {unix,_} = OS} ->
            {ok, Child} = alcove:fork(Drv),
            #state{
                pid = Drv,
                child = Child,
                os = OS
            }
    end.

stop(#state{pid = Drv}) ->
    alcove_drv:stop(Drv).

msg(_) ->
    % Length, Message type, Term
    Msg = <<
        0,37, 0,3, 0,0,1,39,
        0,29, 0,3, 0,0,2,39,
        0,21, 0,3, 0,0,3,39,
        0,13, 0,4, 131,109,0,0,0,5,48,46,50,46,48
        >>,

    Reply = alcove_codec:decode(Msg),

    ?_assertEqual(
        {alcove_call,[295,551,807],<<"0.2.0">>},
        Reply
    ).

version(#state{pid = Drv}) ->
    Version = alcove:version(Drv),
    ?_assertEqual(true, is_binary(Version)).

iodata(#state{pid = Drv}) ->
    Iolist = ["0",1,"2",3,
              [4,[5,6,<<7,8,9>>,["10"]]],
              <<"1112, 13, 14, 15">>,
              [16,17,"18,19,20"],
              21,
              <<22>>],

    Reply0 = alcove:iolist_to_bin(Drv, Iolist),

    % Valid iolists: binary, string, lists, bytes must be within a list
    Reply1 = (catch alcove:iolist_to_bin(Drv, 10)),
    Reply2 = (catch alcove:iolist_to_bin(Drv, [123456])),
    Reply3 = alcove:iolist_to_bin(Drv, <<1,2,3,4,5,6>>),
    Reply4 = alcove:iolist_to_bin(Drv, "ok"),

    % Arbitrary implementation limit of 16 nested
    % lists. iolist_to_binary/1 does not have this limitation.
    Reply5 = alcove:iolist_to_bin(Drv, [[[[[[[[[[[[[[[["ok"]]]]]]]]]]]]]]]]),
    Reply6 = (catch alcove:iolist_to_bin(Drv, [[[[[[[[[[[[[[[[["fail"]]]]]]]]]]]]]]]]])),

    [
        ?_assertEqual(Reply0, iolist_to_binary(Iolist)),
        ?_assertMatch({'EXIT',{badarg,_}}, Reply1),
        ?_assertMatch({'EXIT',{badarg,_}}, Reply2),
        ?_assertEqual(<<1,2,3,4,5,6>>, Reply3),
        ?_assertEqual(<<"ok">>, Reply4),
        ?_assertEqual(<<"ok">>, Reply5),
        ?_assertMatch({'EXIT',{badarg,_}}, Reply6)
    ].

pid(#state{pid = Drv}) ->
    Pids = alcove:pid(Drv),
    ?_assertEqual(1, length(Pids)).

getpid(#state{clone = true, pid = Drv, child = Child}) ->
    % Running in a PID namespace
    PID = alcove:getpid(Drv, [Child]),
    ?_assertEqual(1, PID);
getpid(#state{pid = Drv, child = Child}) ->
    PID = alcove:getpid(Drv, [Child]),
    ?_assertEqual(true, PID > 0).

setopt(#state{pid = Drv}) ->
    {ok, Fork} = alcove:fork(Drv),

    true = alcove:setopt(Drv, [Fork], maxchild, 128),

    {ok, Fork1} = alcove:fork(Drv, [Fork]),

    Opt1 = alcove:getopt(Drv, [Fork], maxchild),
    Opt2 = alcove:getopt(Drv, [Fork, Fork1], maxchild),

    alcove:exit(Drv, [Fork, Fork1], 0),

    true = alcove:setopt(Drv, [Fork], exit_status, 0),
    true = alcove:setopt(Drv, [Fork], maxforkdepth, 0),

    Opt3 = alcove:getopt(Drv, [Fork], exit_status),
    Opt4 = alcove:getopt(Drv, [], maxforkdepth),
    Opt5 = alcove:getopt(Drv, [Fork], maxforkdepth),
    Reply = alcove:fork(Drv, [Fork]),

    alcove:exit(Drv, [Fork], 0),

    [
        ?_assertEqual(128, Opt1),
        ?_assertEqual(128, Opt2),
        ?_assertEqual(0, Opt3),
        ?_assertNotEqual(0, Opt4),
        ?_assertEqual(0, Opt5),
        ?_assertEqual({error,eagain}, Reply)
    ].

event(#state{pid = Drv}) ->
    {ok, Fork} = alcove:fork(Drv),
    Reply0 = alcove:exit(Drv, [Fork], 0),
    Reply1 = alcove:event(Drv, [Fork], 5000),
    Reply2 = alcove:event(Drv, [], 5000),

    [
        ?_assertEqual(ok, Reply0),
        ?_assertEqual({exit_status,0}, Reply1),
        ?_assertMatch({signal,_}, Reply2)
    ].

sethostname(#state{clone = true, pid = Drv, child = Child}) ->
    Reply = alcove:sethostname(Drv, [Child], "alcove"),
    Hostname = alcove:gethostname(Drv, [Child]),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"alcove">>}, Hostname)];
sethostname(#state{pid = Drv, child = Child}) ->
    Hostname = alcove:gethostname(Drv, [Child]),
    ?_assertMatch({ok, <<_/binary>>}, Hostname).

env(#state{pid = Drv, child = Child}) ->
    Reply0 = alcove:getenv(Drv, [Child], "ALCOVE"),
    Reply1 = alcove:setenv(Drv, [Child], "ALCOVE", "12345", 0),
    Reply2 = alcove:getenv(Drv, [Child], "ALCOVE"),
    Reply3 = alcove:setenv(Drv, [Child], "ALCOVE", "abcd", 1),
    Reply4 = alcove:getenv(Drv, [Child], "ALCOVE"),
    Reply5 = alcove:unsetenv(Drv, [Child], "ALCOVE"),
    Reply6 = alcove:getenv(Drv, [Child], "ALCOVE"),

    Reply7 = alcove:environ(Drv, [Child]),
    Reply8 = alcove:clearenv(Drv, [Child]),
    Reply9 = alcove:environ(Drv, [Child]),

    [
        ?_assertEqual(false, Reply0),
        ?_assertEqual(ok, Reply1),
        ?_assertEqual(<<"12345">>, Reply2),
        ?_assertEqual(ok, Reply3),
        ?_assertEqual(<<"abcd">>, Reply4),
        ?_assertEqual(ok, Reply5),
        ?_assertEqual(false, Reply6),

        ?_assertNotEqual(0, length(Reply7)),
        ?_assertEqual(ok, Reply8),
        ?_assertEqual(0, length(Reply9))
    ].

clone_define(#state{clone = true, pid = Drv, child = Child}) ->
    Reply = alcove:clone_define(Drv, [Child], clone_newns),
    ?_assertEqual(true, is_integer(Reply));
clone_define(_) ->
    [].

setns(#state{clone = true, pid = Drv, child = Child}) ->
    {ok, Child1} = alcove:fork(Drv),
    ok = alcove:setns(Drv, [Child1], [
            "/proc/",
            integer_to_list(Child),
            "/ns/uts"
        ]),
    Hostname0 = alcove:gethostname(Drv, [Child]),
    Hostname1 = alcove:gethostname(Drv, [Child1]),
    ?_assertEqual(Hostname0, Hostname1);
setns(_) ->
    [].

unshare(#state{clone = true, pid = Drv}) ->
    Host = alcove:gethostname(Drv),
    {ok, Child1} = alcove:fork(Drv),
    ok = alcove:unshare(Drv, [Child1], [clone_newuts]),
    Reply = alcove:sethostname(Drv, [Child1], "unshare"),
    Hostname = alcove:gethostname(Drv, [Child1]),
    Host = alcove:gethostname(Drv),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"unshare">>}, Hostname)];
unshare(_) ->
    [].

mount_define(#state{os = {unix,sunos}, pid = Drv}) ->
    Flags = alcove:define(Drv, [
            rdonly,
            nosuid
        ]),
    ?_assertEqual(true, is_integer(Flags));
mount_define(#state{pid = Drv}) ->
    Flags = alcove:define(Drv, [
            rdonly,
            nosuid,
            noexec,
            noatime
        ]),
    ?_assertEqual(true, is_integer(Flags)).

mount(#state{clone = true, pid = Drv, child = Child}) ->
    Mount = alcove:mount(Drv, [Child], "/tmp", "/mnt", "", [
            ms_bind,
            ms_rdonly,
            ms_noexec
        ], "", ""),
    Umount = alcove:umount(Drv, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
mount(_) ->
    [].

tmpfs(#state{clone = true, pid = Drv, child = Child}) ->
    Mount = alcove:mount(Drv, [Child], "tmpfs", "/mnt", "tmpfs", [ms_noexec], <<"size=16M", 0>>, <<>>),
    Umount = alcove:umount(Drv, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
tmpfs(#state{os = {unix,OS}, pid = Drv, child = Child}) when OS =:= linux; OS =:= openbsd ->
    % Linux: running in a fork in the global namespace
    Dir = "/tmp/alcove." ++ [ crypto:rand_uniform(16#30,16#39) || _ <- lists:seq(1,8) ],
    ok = alcove:mkdir(Drv, [Child], Dir, 8#700),
    Mount = alcove:mount(Drv, [Child], "tmpfs", Dir, "tmpfs", [ms_noexec],
        <<"size=16M", 0>>, <<>>),
    Umount = alcove:umount(Drv, [Child], Dir),
    Rmdir = alcove:rmdir(Drv, [Child], Dir),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount),
        ?_assertEqual(ok, Rmdir)
    ];
tmpfs(#state{os = {unix,sunos}, pid = Drv, child = Child}) ->
    Dir = "/tmp/alcove." ++ [ crypto:rand_uniform(16#30,16#39) || _ <- lists:seq(1,8) ],
    ok = alcove:mkdir(Drv, [Child], Dir, 8#700),
    Mount = alcove:mount(Drv, [Child], "swap", Dir, "tmpfs", [ms_optionstr],
        <<>>, <<"size=16m", 0:4096>>),
    Umount = alcove:umount(Drv, [Child], Dir),
    Rmdir = alcove:rmdir(Drv, [Child], Dir),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount),
        ?_assertEqual(ok, Rmdir)
    ];
tmpfs(_) ->
    [].

chroot(#state{os = {unix,OS}, pid = Drv, child = Child}) when OS =:= linux; OS =:= openbsd ->
    Reply = alcove:chroot(Drv, [Child], "/bin"),
    ?_assertEqual(ok, Reply);
chroot(#state{os = {unix,OS}, pid = Drv, child = Child}) when OS =:= freebsd; OS =:= netbsd ->
    Reply = alcove:chroot(Drv, [Child], "/rescue"),
    ?_assertEqual(ok, Reply);
chroot(_) ->
    [].

chdir(#state{pid = Drv, child = Child}) ->
    Reply = alcove:chdir(Drv, [Child], "/"),
    CWD = alcove:getcwd(Drv, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"/">>}, CWD)
    ].

setrlimit(#state{pid = Drv, child = Child}) ->
    Reply = alcove:setrlimit(Drv, [Child], rlimit_nofile, #alcove_rlimit{cur = 64, max = 64}),
    Rlimit = alcove:getrlimit(Drv, [Child], rlimit_nofile),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, #alcove_rlimit{cur = 64, max = 64}}, Rlimit)
    ].

setgid(#state{pid = Drv, child = Child}) ->
    Reply = alcove:setgid(Drv, [Child], 65534),
    GID = alcove:getgid(Drv, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, GID)
    ].

setuid(#state{pid = Drv, child = Child}) ->
    Reply = alcove:setuid(Drv, [Child], 65534),
    UID = alcove:getuid(Drv, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, UID)
    ].

fork(#state{pid = Drv, child = Child}) ->
    Pids = [ alcove:fork(Drv, [Child]) || _ <- lists:seq(1, 32) ],
    [Last|_Rest] = lists:reverse(Pids),
    Reply0 = alcove:getpid(Drv, [Child]),

    [{ok, Child0}|_] = [ N || N <- Pids, N =/= {error,eagain} ],
    ok = alcove:exit(Drv, [Child, Child0], 0),
    waitpid(Drv, [Child], Child0),
    Reply1 = alcove:fork(Drv, [Child]),
    Reply2 = alcove:fork(Drv, [Child]),

    % XXX discard output from killed process
    flush(stdout, Drv, [Child]),

    [
        ?_assertEqual(true, is_integer(Reply0)),
        ?_assertEqual({error,eagain}, Last),

        ?_assertMatch({ok, _}, Reply1),
        ?_assertEqual({error,eagain}, Reply2)
    ].

badpid(#state{pid = Drv}) ->
    {ok, Child} = alcove:fork(Drv),

    % EPIPE or PID not found
    ok = alcove:execvp(Drv, [Child], "/bin/sh",
        ["/bin/sh", "-c", "echo > /dev/null"]),
    waitpid_exit(Drv, [], Child),
    Reply0 = (catch alcove:execvp(Drv, [Child],
            "/bin/sh", ["/bin/sh", "-c", "echo > /dev/null"])),

    % PID not found
    PID = get_unused_pid(Drv),
    Reply1 = (catch alcove:execvp(Drv, [PID],
            "/bin/sh", ["/bin/sh", "-c", "echo > /dev/null"])),

    % Invalid PIDs
    Reply2 = (catch alcove:execvp(Drv, [0],
            "/bin/sh", ["/bin/sh", "-c", "echo > /dev/null"])),

    [
        ?_assertEqual({'EXIT',badpid}, Reply0),
        ?_assertEqual({'EXIT',badpid}, Reply1),
        ?_assertEqual({'EXIT',badpid}, Reply2)
    ].

signal(#state{pid = Drv}) ->
    {ok, Child1} = alcove:fork(Drv),

    SA0 = alcove:sigaction(Drv, [Child1], sigterm, sig_ign),
    Kill0 = alcove:kill(Drv, Child1, sigterm),
    Pid0 = alcove:getpid(Drv, [Child1]),

    SA1 = alcove:sigaction(Drv, [Child1], sigterm, sig_dfl),
    Kill1 = alcove:kill(Drv, Child1, sigterm),
    waitpid(Drv, [], Child1),
    alcove:kill(Drv, Child1, 0),
    Search = alcove:kill(Drv, Child1, 0),

    [
        ?_assertEqual(ok, SA0),
        ?_assertEqual(ok, Kill0),
        ?_assertEqual(true, is_integer(Pid0)),

        ?_assertEqual(ok, SA1),
        ?_assertEqual(ok, Kill1),
        ?_assertEqual({error,esrch}, Search)
    ].

portstress(#state{pid = Drv, child = Child}) ->
    Reply = [ alcove:version(Drv, [Child]) || _ <- lists:seq(1,1000) ],
    Ok = lists:filter(fun
            (false) -> false;
            (_) -> true
            end, Reply),
    ?_assertEqual(Ok, Reply).

forkstress(#state{pid = Drv}) ->
    {ok, Fork} = alcove:fork(Drv),
    Reply = forkstress_1(Drv, Fork, 100),
    ?_assertEqual(ok, Reply).

forkchain(#state{pid = Drv}) ->
    {ok, Child0} = alcove:fork(Drv),
    {ok, Child1} = alcove:fork(Drv, [Child0]),
    {ok, Child2} = alcove:fork(Drv, [Child0, Child1]),
    {ok, Child3} = alcove:fork(Drv, [Child0, Child1, Child2]),
    {ok, Child4} = alcove:fork(Drv, [Child0, Child1, Child2, Child3]),

    Pid = alcove:getpid(Drv, [Child0, Child1, Child2, Child3, Child4]),

    alcove:exit(Drv, [Child0], 0),

    ?_assertEqual(Pid, Child4).

eof(#state{pid = Drv}) ->
    {ok, Child} = alcove:fork(Drv),
    {ok, Child0} = alcove:fork(Drv, [Child]),

    ok = alcove:eof(Drv, [Child,Child0], stderr),
    Reply0 = alcove:eof(Drv, [Child,Child0], stderr),

    ok = alcove:eof(Drv, [Child,Child0], stdout),
    Reply1 = alcove:eof(Drv, [Child,Child0], stdout),

    ok = alcove:eof(Drv, [Child,Child0]),
    Reply2 = case alcove:eof(Drv, [Child,Child0]) of
        {error,esrch} -> ok;
        {error,ebadf} -> ok;
        N -> N
    end,

    alcove:exit(Drv, [Child], 0),

    [
        ?_assertEqual({error,ebadf}, Reply0),
        ?_assertEqual({error,ebadf}, Reply1),
        ?_assertEqual(ok, Reply2)
    ].

alloc(#state{os = {unix,_}, pid = Drv}) ->
    {ok, Buf, Cstruct} = alcove:alloc(Drv,
        [<<1,2,3,4,5,6,7,8,9,10>>,
         {ptr, 11},
         <<11,12,13,14,15>>,
         {ptr, <<16,17,18,19,20,21,22>>},
         <<23,24,25>>]
    ),
    Size = erlang:system_info({wordsize,external}),
    Buflen = 10 + Size + 5 + Size + 3,
    [
        ?_assertEqual(Buflen, byte_size(Buf)),
        ?_assertEqual(lists:nth(1, Cstruct), <<1,2,3,4,5,6,7,8,9,10>>),
        ?_assertEqual(lists:nth(2, Cstruct), {ptr, <<0:88>>}),
        ?_assertEqual(lists:nth(3, Cstruct), <<11,12,13,14,15>>),
        ?_assertEqual(lists:nth(4, Cstruct), {ptr, <<16,17,18,19,20,21,22>>}),
        ?_assertEqual(lists:nth(5, Cstruct), <<23,24,25>>)
    ].

prctl(#state{os = {unix,linux}, pid = Drv}) ->
    {ok, Fork} = alcove:fork(Drv),

    % capability is set:
    %   returns 0 | 1 in function result, arg2 = int
    Reply0 = alcove:prctl(Drv, [Fork], pr_capbset_read, 0, 0,0,0),

    % set process name:
    %   arg2 = char *, up to 16 bytes, NULL terminated
    Reply1 = alcove:prctl(Drv, [Fork], pr_set_name, <<"test",0>>, 0,0,0),

    % get process name
    %   value returned in arg2 = char *, up to 16 bytes
    Reply2 = alcove:prctl(Drv, [Fork], pr_get_name, <<0:(17*8)>>, 0,0,0),

    % set parent death signal
    %  arg2 = signal
    Reply3 = alcove:prctl(Drv, [Fork], pr_set_pdeathsig, 9, 0,0,0),

    % get parent death signal
    %  arg2 = int *
    Reply4 = alcove:prctl(Drv, [Fork], pr_get_pdeathsig, <<0:32>>, 0,0,0),

    [
        ?_assertEqual({ok,1,0,0,0,0}, Reply0),
        ?_assertEqual({ok,0,<<116,101,115,116,0>>,0,0,0}, Reply1),
        ?_assertMatch({ok,0,<<116,101,115,116,0,0,0,0,0,0,0,0,0,0,0,0,0>>,0,0,0}, Reply2),
        ?_assertMatch({ok,0,9,0,0,0}, Reply3),
        ?_assertMatch({ok,0,<<9,0,0,0>>,0,0,0}, Reply4)
    ];
prctl(_) ->
    [].

priority(#state{os = {unix,_}, pid = Drv}) ->
    {ok, Fork0} = alcove:fork(Drv),
    {ok, Fork1} = alcove:fork(Drv, [Fork0]),
    {ok, Fork2} = alcove:fork(Drv, [Fork0]),

    Prio0 = alcove:getpriority(Drv, [Fork0,Fork1], prio_process, 0),
    ok = alcove:setpriority(Drv, [Fork0,Fork1], prio_process, 0, 10),
    Prio1 = alcove:getpriority(Drv, [Fork0,Fork1], prio_process, 0),

    case alcove:getrlimit(Drv, [Fork0,Fork1], rlimit_nice) of
        {error,einval} ->
            ok;
        {ok, #alcove_rlimit{cur = Cur}} when Cur =:= 0 ->
            ok;
        {ok, #alcove_rlimit{}} ->
            alcove:setrlimit(Drv, [Fork0,Fork1], rlimit_nice,
                #alcove_rlimit{cur = 0, max = 0})
    end,

    ok = alcove:setuid(Drv, [Fork0,Fork1], 65534),
    Eacces = alcove:setpriority(Drv, [Fork0,Fork1], prio_process, 0, 1),

    ok = alcove:setpriority(Drv, [Fork0,Fork2], prio_process, 0, -1),
    Prio2 = alcove:getpriority(Drv, [Fork0,Fork2], prio_process, 0),

    alcove:exit(Drv, [Fork0], 0),

    [
        ?_assertEqual({ok,0}, Prio0),
        ?_assertEqual({ok,10}, Prio1),
        ?_assertEqual({error,eacces}, Eacces),
        ?_assertEqual({ok,-1}, Prio2)
    ].

execvp(#state{os = {unix,linux}, pid = Drv, child = Child}) ->
    % cwd = /, chroot'ed in /bin
    Reply = alcove:execvp(Drv, [Child], "/busybox", ["/busybox", "sh"]),
    ?_assertEqual(ok, Reply);
execvp(#state{os = {unix,OS}, pid = Drv, child = Child}) when OS =:= freebsd; OS =:= openbsd; OS =:= netbsd ->
    % cwd = /, chroot'ed in /rescue
    Reply = alcove:execvp(Drv, [Child], "/sh", ["/sh"]),
    ?_assertEqual(ok, Reply);
execvp(#state{os = {unix,OS}, pid = Drv, child = Child}) when OS =:= sunos ->
    % not in a chroot
    Reply = alcove:execvp(Drv, [Child], "/bin/sh", ["/bin/sh"]),
    ?_assertEqual(ok, Reply);
execvp(_) ->
    [].

execvp_with_signal(#state{pid = Drv}) ->
    {ok, Fork} = alcove:fork(Drv),
    Reply0 = (catch alcove:execvp(Drv, [Fork], "/bin/sh",
            ["/bin/sh", "-c", "kill -9 $$"])),
    Reply1 = alcove:event(Drv, [Fork], 5000),

    [
        ?_assertEqual(ok, Reply0),
        ?_assertEqual({termsig,sigkill}, Reply1)
    ].

stdout(#state{pid = Drv, child = Child}) ->
    Reply = alcove:stdin(Drv, [Child], "echo 0123456789\n"),
    Stdout = alcove:stdout(Drv, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertEqual(<<"0123456789\n">>, Stdout)
    ].

stderr(#state{os = {unix,linux}, pid = Drv, child = Child}) ->
    Reply = alcove:stdin(Drv, [Child], "nonexistent 0123456789\n"),
    Stderr = alcove:stderr(Drv, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertMatch(<<"sh: ", _/binary>>, Stderr)
    ];
stderr(#state{os = {unix,OS}, pid = Drv, child = Child}) when OS =:= freebsd; OS =:= netbsd ->
    Reply = alcove:stdin(Drv, [Child], "nonexistent 0123456789\n"),
    Stderr = alcove:stderr(Drv, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertEqual(<<"nonexistent: not found\n">>, Stderr)
    ];
stderr(#state{os = {unix,openbsd}, pid = Drv, child = Child}) ->
    Reply = alcove:stdin(Drv, [Child], "nonexistent 0123456789\n"),
    Stderr = alcove:stderr(Drv, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertMatch(<<"/sh: ", _/binary>>, Stderr)
    ];
stderr(_) ->
    [].

execve(#state{pid = Drv}) ->
    {ok, Child0} = alcove:fork(Drv),
    {ok, Child1} = alcove:fork(Drv),

    Reply0 = (catch alcove:execve(Drv, [Child0], "/usr/bin/env",
            ["/usr/bin/env"], ["A=1", "B=2", "C=3", false])),
    Reply1 = alcove:execve(Drv, [Child0], "/usr/bin/env",
        ["/usr/bin/env"], ["FOO=bar", "BAR=1234567"]),
    Reply2 = alcove:execve(Drv, [Child1], "/usr/bin/env",
        ["/usr/bin/env"], []),

    Stdout0 = alcove:stdout(Drv, [Child0], 5000),
    Stdout1 = alcove:stdout(Drv, [Child1], 5000),

    [
        ?_assertMatch({'EXIT',{badarg,_}}, Reply0),
        ?_assertEqual(ok, Reply1),
        ?_assertEqual(ok, Reply2),

        ?_assertEqual(<<"FOO=bar\nBAR=1234567\n">>, Stdout0),
        ?_assertEqual(false, Stdout1)
    ].

stream(#state{pid = Drv}) ->
    Chain = chain(Drv, 16),
    ok = alcove:sigaction(Drv, Chain, sigpipe, sig_dfl),
    DefaultCount = 1 * 1024 * 1024,
    Count = getenv("ALCOVE_TEST_STREAM_COUNT", integer_to_list(DefaultCount)),
    Sleep = getenv("ALCOVE_TEST_STREAM_MAGIC_SLEEP", "0"),
    % XXX procs in the fork path may exit before all the data has
    % XXX been written
    Cmd = ["yes | head -", Count, ";sleep ", Sleep],
    ok = alcove:execvp(Drv, Chain, "/bin/sh", ["/bin/sh", "-c", Cmd]),
    % <<"y\n">>
    Reply = stream_count(Drv, Chain, list_to_integer(Count)*2),
    ?_assertEqual(ok, Reply).

open(#state{pid = Drv}) ->
    O_RDONLY = alcove:define(Drv, o_rdonly),

    File = "/nonexistent",

    Reply0 = alcove:open(Drv, File, O_RDONLY, 0),
    Reply1 = alcove:open(Drv, File, [O_RDONLY,O_RDONLY,O_RDONLY,O_RDONLY], 0),
    Reply2 = alcove:open(Drv, File, [o_rdonly, o_rdonly, o_rdonly], 0),

    [
        ?_assertEqual({error,enoent}, Reply0),
        ?_assertEqual({error,enoent}, Reply1),
        ?_assertEqual({error,enoent}, Reply2)
    ].

execvp_mid_chain(#state{os = {unix,OS}, pid = Drv}) ->
    Chain = chain(Drv, 8),
    {Pids, Rest} = lists:split(3, Chain),
    ok = alcove:execvp(Drv, Pids, "/bin/cat", ["/bin/cat"]),

    alcove:stdin(Drv, Pids, "test\n"),
    Reply0 = alcove:stdout(Drv, Pids, 5000),
    waitpid_exit(Drv, [], lists:last(Rest)),
    Reply1 = [ alcove:kill(Drv, Pid, 0) || Pid <- Rest ],

    ChildState = case OS of
        openbsd -> {error,esrch};
        _ -> ok
    end,

    % The child spawned by the exec'ed process becomes a zombie
    % because the PID will not be reaped.
    [
        ?_assertEqual(<<"test\n">>, Reply0),
        ?_assertEqual([
                ChildState,
                {error,esrch},
                {error,esrch},
                {error,esrch},
                {error,esrch}
            ], Reply1)
    ].

%%
%% Utility functions
%%
getenv(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        Env -> Env
    end.

waitpid(Drv, Pids, Child) ->
    case alcove:event(Drv, Pids, 5000) of
        {signal, sigchld} ->
            waitpid_exit(Drv, Pids, Child);
        false ->
            false
    end.

waitpid_exit(Drv, Pids, Child) ->
    case alcove:kill(Drv, Pids, Child, 0) of
        ok ->
            timer:sleep(10),
            waitpid_exit(Drv, Pids, Child);
        {error,esrch} ->
            ok
    end.

flush(stdout, Drv, Pids) ->
    case alcove:stdout(Drv, Pids) of
        false ->
            ok;
        _ ->
            flush(stdout, Drv, Pids)
    end.

get_unused_pid(Drv) ->
    PID = crypto:rand_uniform(16#0affffff, 16#0fffffff),
    case alcove:kill(Drv, PID, 0) of
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
