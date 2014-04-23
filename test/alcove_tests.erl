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
        pid(State),
        getpid(State),
        setopt(State),
        event(State),
        sethostname(State),
        env(State),
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
        signal(State),
        portstress(State),
        forkstress(State),
        forkchain(State),
        eof(State),
        prctl(State),
        execvp(State),
        stdout(State),
        stderr(State),
        execve(State)
    ].

start() ->
    % export ALCOVE_TEST_EXEC="sudo valgrind --leak-check=yes --log-file=/tmp/alcove.log"
    Exec = case os:getenv("ALCOVE_TEST_EXEC") of
        false -> "sudo";
        Env -> Env
    end,

    Use_fork = case os:getenv("ALCOVE_TEST_USE_FORK") of
        false -> false;
        _ -> true
    end,

    {ok, Drv} = alcove_drv:start([{exec, Exec}, {maxchild, 8}]),

    ok = alcove:setopt(Drv, sigchld, 1),

    case {Use_fork, os:type()} of
        {false, {unix,linux} = OS} ->
            Flags = alcove:define(Drv, [
                    'CLONE_NEWIPC',
                    'CLONE_NEWNET',
                    'CLONE_NEWNS',
                    'CLONE_NEWPID',
                    'CLONE_NEWUTS'
                ]),
            {ok, Child} = alcove:clone(Drv, Flags),
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
    % The length of the first message is stripped off by erlang
    Msg = <<
              0,3, 0,0,1,39,
        0,47, 0,3, 0,0,2,39,
        0,39, 0,3, 0,0,3,39,
        0,13, 0,4, 131,109,0,0,0,5,48,46,50,46,48,
        0,16, 0,3, 0,0,3,46,
        0,8,  0,4, 131,100,0,2,111,107
        >>,

    Reply = alcove_drv:decode(Msg),

    ?_assertEqual(
        [{alcove_call,[295,551,807],<<"0.2.0">>},
            {alcove_call,[814],ok}],
        Reply
    ).

version(#state{pid = Drv}) ->
    Version = alcove:version(Drv),
    ?_assertEqual(true, is_binary(Version)).

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

    ok = alcove:setopt(Drv, [Fork], maxchild, 128),

    {ok, Fork1} = alcove:fork(Drv, [Fork]),

    Opt1 = alcove:getopt(Drv, [Fork], maxchild),
    Opt2 = alcove:getopt(Drv, [Fork, Fork1], maxchild),

    alcove:exit(Drv, [Fork, Fork1], 0),

    ok = alcove:setopt(Drv, [Fork], exit_status, 0),
    ok = alcove:setopt(Drv, [Fork], maxforkdepth, 0),

    Opt3 = alcove:getopt(Drv, [Fork], exit_status),
    Opt4 = alcove:getopt(Drv, [], maxforkdepth),
    Opt5 = alcove:getopt(Drv, [Fork], maxforkdepth),
    Reply = alcove:fork(Drv, [Fork]),

    ok = alcove:setopt(Drv, [Fork], verbose, 2),
    Fork = alcove:getpid(Drv, [Fork]),
    Stderr = alcove:stderr(Drv, [Fork]),

    alcove:exit(Drv, [Fork], 0),

    [
        ?_assertEqual(128, Opt1),
        ?_assertEqual(128, Opt2),
        ?_assertEqual(0, Opt3),
        ?_assertNotEqual(0, Opt4),
        ?_assertEqual(0, Opt5),
        ?_assertEqual({error,eagain}, Reply),
        ?_assertNotEqual(false, Stderr)
    ].

event(#state{pid = Drv}) ->
    {ok, Fork} = alcove:fork(Drv),
    Reply0 = alcove:exit(Drv, [Fork], 0),
    Reply1 = alcove:event(Drv, [Fork]),
    Reply2 = alcove:event(Drv, []),

    [
        ?_assertEqual(ok, Reply0),
        ?_assertEqual(false, Reply1),
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
    ?_assertEqual(ok,ok).

unshare(#state{clone = true, pid = Drv}) ->
    {ok, Child1} = alcove:fork(Drv),
    ok = alcove:unshare(Drv, [Child1],
        alcove:clone_define(Drv, 'CLONE_NEWUTS')),
    Reply = alcove:sethostname(Drv, [Child1], "unshare"),
    Hostname = alcove:gethostname(Drv, [Child1]),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"unshare">>}, Hostname)];
unshare(_) ->
    ?_assertEqual(ok,ok).

mount_define(#state{pid = Drv}) ->
    Flags = alcove:define(Drv, [
            rdonly,
            nosuid,
            noexec,
            noatime
        ]),
    ?_assertEqual(true, is_integer(Flags)).

mount(#state{clone = true, pid = Drv, child = Child}) ->
    Flags = alcove:define(Drv, [
            'MS_BIND',
            'MS_RDONLY',
            'MS_NOEXEC'
            ]),
    Mount = alcove:mount(Drv, [Child], "/tmp", "/mnt", "", Flags, ""),
    Umount = alcove:umount(Drv, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
mount(_) ->
    ?_assertEqual(ok,ok).

tmpfs(#state{clone = true, pid = Drv, child = Child}) ->
    Flags = alcove:define(Drv, ['MS_NOEXEC']),
    Mount = alcove:mount(Drv, [Child], "tmpfs", "/mnt", "tmpfs", Flags, <<"size=16M", 0>>),
    Umount = alcove:umount(Drv, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
tmpfs(_) ->
    ?_assertEqual(ok,ok).

chroot(#state{os = {unix,linux}, pid = Drv, child = Child}) ->
    Reply = alcove:chroot(Drv, [Child], "/bin"),
    ?_assertEqual(ok, Reply);
chroot(#state{os = {unix,freebsd}, pid = Drv, child = Child}) ->
    Reply = alcove:chroot(Drv, [Child], "/rescue"),
    ?_assertEqual(ok, Reply).

chdir(#state{pid = Drv, child = Child}) ->
    Reply = alcove:chdir(Drv, [Child], "/"),
    CWD = alcove:getcwd(Drv, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"/">>}, CWD)
    ].

setrlimit(#state{pid = Drv, child = Child}) ->
    RLIMIT_NOFILE = alcove:rlimit_define(Drv, 'RLIMIT_NOFILE'),
    Reply = alcove:setrlimit(Drv, [Child], RLIMIT_NOFILE, #rlimit{cur = 64, max = 64}),
    Rlimit = alcove:getrlimit(Drv, [Child], RLIMIT_NOFILE),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, #rlimit{cur = 64, max = 64}}, Rlimit)
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

signal(#state{pid = Drv}) ->
    {ok, Child1} = alcove:fork(Drv),

    TERM = alcove:signal_define(Drv, 'SIGTERM'),

    SA0 = alcove:sigaction(Drv, [Child1], TERM, ign),
    Kill0 = alcove:kill(Drv, Child1, TERM),
    Pid0 = alcove:getpid(Drv, [Child1]),

    SA1 = alcove:sigaction(Drv, [Child1], TERM, dfl),
    Kill1 = alcove:kill(Drv, Child1, TERM),
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

prctl(#state{os = {unix,linux}, pid = Drv}) ->
    {ok, Fork} = alcove:fork(Drv),

    % capability is set:
    %   returns 0 | 1 in function result, arg2 = int
    PR_CAPBSET_READ = alcove:prctl_define(Drv, 'PR_CAPBSET_READ'),
    Reply0 = alcove:prctl(Drv, [Fork], PR_CAPBSET_READ, 0, 0,0,0),

    % set process name:
    %   arg2 = char *, up to 16 bytes, NULL terminated
    PR_SET_NAME = alcove:prctl_define(Drv, 'PR_SET_NAME'),
    Reply1 = alcove:prctl(Drv, [Fork], PR_SET_NAME, <<"test",0>>, 0,0,0),

    % get process name
    %   value returned in arg2 = char *, up to 16 bytes
    PR_GET_NAME = alcove:prctl_define(Drv, 'PR_GET_NAME'),
    Reply2 = alcove:prctl(Drv, [Fork], PR_GET_NAME, <<0:(17*8)>>, 0,0,0),

    % set parent death signal
    %  arg2 = signal
    PR_SET_PDEATHSIG = alcove:prctl_define(Drv, 'PR_SET_PDEATHSIG'),
    Reply3 = alcove:prctl(Drv, [Fork], PR_SET_PDEATHSIG, 9, 0,0,0),

    % get parent death signal
    %  arg2 = int *
    PR_GET_PDEATHSIG = alcove:prctl_define(Drv, 'PR_GET_PDEATHSIG'),
    Reply4 = alcove:prctl(Drv, [Fork], PR_GET_PDEATHSIG, <<0:32>>, 0,0,0),

    [
        ?_assertEqual({ok,1,0,0,0,0}, Reply0),
        ?_assertEqual({ok,0,<<116,101,115,116,0>>,0,0,0}, Reply1),
        ?_assertMatch({ok,0,<<116,101,115,116,0,0,0,0,0,0,0,0,0,0,0,0,0>>,0,0,0}, Reply2),
        ?_assertMatch({ok,0,9,0,0,0}, Reply3),
        ?_assertMatch({ok,0,<<9,0,0,0>>,0,0,0}, Reply4)
    ];
prctl(_) ->
    ?_assertEqual(ok,ok).


execvp(#state{os = {unix,linux}, pid = Drv, child = Child}) ->
    % cwd = /, chroot'ed in /bin
    Reply = alcove:execvp(Drv, [Child], "/busybox", ["/busybox", "sh"]),
    ?_assertEqual(ok, Reply);
execvp(#state{os = {unix,freebsd}, pid = Drv, child = Child}) ->
    % cwd = /, chroot'ed in /rescue
    Reply = alcove:execvp(Drv, [Child], "/sh", ["/sh"]),
    ?_assertEqual(ok, Reply).

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
stderr(#state{os = {unix,freebsd}, pid = Drv, child = Child}) ->
    Reply = alcove:stdin(Drv, [Child], "nonexistent 0123456789\n"),
    Stderr = alcove:stderr(Drv, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertEqual(<<"nonexistent: not found\n">>, Stderr)
    ].

execve(#state{pid = Drv}) ->
    {ok, Child0} = alcove:fork(Drv),
    {ok, Child1} = alcove:fork(Drv),

    Reply0 = alcove:execve(Drv, [Child0], "/usr/bin/env",
        ["/usr/bin/env"], ["FOO=bar", "BAR=1234567"]),
    Reply1 = alcove:execve(Drv, [Child1], "/usr/bin/env",
        ["/usr/bin/env"], []),

    Stdout0 = alcove:stdout(Drv, [Child0], 5000),
    Stdout1 = alcove:stdout(Drv, [Child1], 5000),

    [
        ?_assertEqual(ok, Reply0),
        ?_assertEqual(ok, Reply1),

        ?_assertEqual(<<"FOO=bar\nBAR=1234567\n">>, Stdout0),
        ?_assertEqual(false, Stdout1)
    ].

%%
%% Utility functions
%%
waitpid(Drv, Pids, Child) ->
    SIGCHLD = alcove:signal_define(Drv, 'SIGCHLD'),
    case alcove:event(Drv, Pids, 5000) of
        {signal, SIGCHLD} ->
            waitpid_1(Drv, Pids, Child);
        false ->
            false
    end.

waitpid_1(Drv, Pids, Child) ->
    case alcove:kill(Drv, Pids, Child, 0) of
        ok ->
            timer:sleep(10),
            waitpid_1(Drv, Pids, Child);
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

forkstress_1(_Drv, _Child, 0) ->
    ok;
forkstress_1(Drv, Child, N) ->
    {ok, Fork} = alcove:fork(Drv, [Child]),
    ok = alcove:exit(Drv, [Child,Fork], 0),
    case alcove:version(Drv, [Child]) of
        {error, timedout} ->
            fail;
        _ ->
            forkstress_1(Drv, Child, N-1)
    end.
