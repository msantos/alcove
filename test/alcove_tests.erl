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
        port,
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

    Port = alcove_drv:start([{exec, Exec}, {maxchild, 8}]),

    case {Use_fork, os:type()} of
        {false, {unix,linux} = OS} ->
            Flags = alcove:define(Port, [
                    'CLONE_NEWIPC',
                    'CLONE_NEWNET',
                    'CLONE_NEWNS',
                    'CLONE_NEWPID',
                    'CLONE_NEWUTS'
                ]),
            {ok, Child} = alcove:clone(Port, Flags),
            #state{
                port = Port,
                child = Child,
                clone = true,
                os = OS
            };
        {_, {unix,_} = OS} ->
            {ok, Child} = alcove:fork(Port),
            #state{
                port = Port,
                child = Child,
                os = OS
            }
    end.

stop(#state{port = Port}) ->
    alcove_drv:stop(Port).

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

version(#state{port = Port}) ->
    Version = alcove:version(Port),
    ?_assertEqual(true, is_binary(Version)).

pid(#state{port = Port}) ->
    Pids = alcove:pid(Port),
    ?_assertEqual(1, length(Pids)).

getpid(#state{clone = true, port = Port, child = Child}) ->
    % Running in a PID namespace
    PID = alcove:getpid(Port, [Child]),
    ?_assertEqual(1, PID);
getpid(#state{port = Port, child = Child}) ->
    PID = alcove:getpid(Port, [Child]),
    ?_assertEqual(true, PID > 0).

setopt(#state{port = Port}) ->
    {ok, Fork} = alcove:fork(Port),

    ok = alcove:setopt(Port, [Fork], maxchild, 128),

    {ok, Fork1} = alcove:fork(Port, [Fork]),

    Opt1 = alcove:getopt(Port, [Fork], maxchild),
    Opt2 = alcove:getopt(Port, [Fork, Fork1], maxchild),

    alcove:exit(Port, [Fork, Fork1], 0),

    ok = alcove:setopt(Port, [Fork], exit_status, 0),
    ok = alcove:setopt(Port, [Fork], maxforkdepth, 0),

    Opt3 = alcove:getopt(Port, [Fork], exit_status),
    Opt4 = alcove:getopt(Port, [], maxforkdepth),
    Opt5 = alcove:getopt(Port, [Fork], maxforkdepth),
    Reply = alcove:fork(Port, [Fork]),

    ok = alcove:setopt(Port, [Fork], verbose, 2),
    Fork = alcove:getpid(Port, [Fork]),
    Stderr = alcove:stderr(Port, [Fork]),

    alcove:exit(Port, [Fork], 0),

    [
        ?_assertEqual(128, Opt1),
        ?_assertEqual(128, Opt2),
        ?_assertEqual(0, Opt3),
        ?_assertNotEqual(0, Opt4),
        ?_assertEqual(0, Opt5),
        ?_assertEqual({error,eagain}, Reply),
        ?_assertNotEqual(false, Stderr)
    ].

event(#state{port = Port}) ->
    {ok, Fork} = alcove:fork(Port),
    Reply0 = alcove:exit(Port, [Fork], 0),
    Reply1 = alcove:event(Port, [Fork]),
    Reply2 = alcove:event(Port, []),

    [
        ?_assertEqual(ok, Reply0),
        ?_assertEqual(false, Reply1),
        ?_assertMatch({signal,_}, Reply2)
    ].

sethostname(#state{clone = true, port = Port, child = Child}) ->
    Reply = alcove:sethostname(Port, [Child], "alcove"),
    Hostname = alcove:gethostname(Port, [Child]),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"alcove">>}, Hostname)];
sethostname(#state{port = Port, child = Child}) ->
    Hostname = alcove:gethostname(Port, [Child]),
    ?_assertMatch({ok, <<_/binary>>}, Hostname).

env(#state{port = Port, child = Child}) ->
    Reply0 = alcove:getenv(Port, [Child], "ALCOVE"),
    Reply1 = alcove:setenv(Port, [Child], "ALCOVE", "12345", 0),
    Reply2 = alcove:getenv(Port, [Child], "ALCOVE"),
    Reply3 = alcove:setenv(Port, [Child], "ALCOVE", "abcd", 1),
    Reply4 = alcove:getenv(Port, [Child], "ALCOVE"),
    Reply5 = alcove:unsetenv(Port, [Child], "ALCOVE"),
    Reply6 = alcove:getenv(Port, [Child], "ALCOVE"),

    Reply7 = alcove:environ(Port, [Child]),
    Reply8 = alcove:clearenv(Port, [Child]),
    Reply9 = alcove:environ(Port, [Child]),

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

setns(#state{clone = true, port = Port, child = Child}) ->
    {ok, Child1} = alcove:fork(Port),
    ok = alcove:setns(Port, [Child1], [
            "/proc/",
            integer_to_list(Child),
            "/ns/uts"
        ]),
    Hostname0 = alcove:gethostname(Port, [Child]),
    Hostname1 = alcove:gethostname(Port, [Child1]),
    ?_assertEqual(Hostname0, Hostname1);
setns(_) ->
    ?_assertEqual(ok,ok).

unshare(#state{clone = true, port = Port}) ->
    {ok, Child1} = alcove:fork(Port),
    ok = alcove:unshare(Port, [Child1],
        alcove:clone_define(Port, 'CLONE_NEWUTS')),
    Reply = alcove:sethostname(Port, [Child1], "unshare"),
    Hostname = alcove:gethostname(Port, [Child1]),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"unshare">>}, Hostname)];
unshare(_) ->
    ?_assertEqual(ok,ok).

mount_define(#state{port = Port}) ->
    Flags = alcove:define(Port, [
            rdonly,
            nosuid,
            noexec,
            noatime
        ]),
    ?_assertEqual(true, is_integer(Flags)).

mount(#state{clone = true, port = Port, child = Child}) ->
    Flags = alcove:define(Port, [
            'MS_BIND',
            'MS_RDONLY',
            'MS_NOEXEC'
            ]),
    Mount = alcove:mount(Port, [Child], "/tmp", "/mnt", "", Flags, ""),
    Umount = alcove:umount(Port, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
mount(_) ->
    ?_assertEqual(ok,ok).

tmpfs(#state{clone = true, port = Port, child = Child}) ->
    Flags = alcove:define(Port, ['MS_NOEXEC']),
    Mount = alcove:mount(Port, [Child], "tmpfs", "/mnt", "tmpfs", Flags, <<"size=16M", 0>>),
    Umount = alcove:umount(Port, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
tmpfs(_) ->
    ?_assertEqual(ok,ok).

chroot(#state{os = {unix,linux}, port = Port, child = Child}) ->
    Reply = alcove:chroot(Port, [Child], "/bin"),
    ?_assertEqual(ok, Reply);
chroot(#state{os = {unix,freebsd}, port = Port, child = Child}) ->
    Reply = alcove:chroot(Port, [Child], "/rescue"),
    ?_assertEqual(ok, Reply).

chdir(#state{port = Port, child = Child}) ->
    Reply = alcove:chdir(Port, [Child], "/"),
    CWD = alcove:getcwd(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"/">>}, CWD)
    ].

setrlimit(#state{port = Port, child = Child}) ->
    RLIMIT_NOFILE = alcove:rlimit_define(Port, 'RLIMIT_NOFILE'),
    Reply = alcove:setrlimit(Port, [Child], RLIMIT_NOFILE, #rlimit{cur = 64, max = 64}),
    Rlimit = alcove:getrlimit(Port, [Child], RLIMIT_NOFILE),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, #rlimit{cur = 64, max = 64}}, Rlimit)
    ].

setgid(#state{port = Port, child = Child}) ->
    Reply = alcove:setgid(Port, [Child], 65534),
    GID = alcove:getgid(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, GID)
    ].

setuid(#state{port = Port, child = Child}) ->
    Reply = alcove:setuid(Port, [Child], 65534),
    UID = alcove:getuid(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, UID)
    ].

fork(#state{port = Port, child = Child}) ->
    Pids = [ alcove:fork(Port, [Child]) || _ <- lists:seq(1, 32) ],
    [Last|_Rest] = lists:reverse(Pids),
    Reply0 = alcove:getpid(Port, [Child]),

    [{ok, Child0}|_] = [ N || N <- Pids, N =/= {error,eagain} ],
    ok = alcove:exit(Port, [Child, Child0], 0),
    waitpid(Port, [Child], Child0),
    Reply1 = alcove:fork(Port, [Child]),
    Reply2 = alcove:fork(Port, [Child]),

    % XXX discard output from killed process
    flush(stdout, Port, [Child]),

    [
        ?_assertEqual(true, is_integer(Reply0)),
        ?_assertEqual({error,eagain}, Last),

        ?_assertMatch({ok, _}, Reply1),
        ?_assertEqual({error,eagain}, Reply2)
    ].

signal(#state{port = Port}) ->
    {ok, Child1} = alcove:fork(Port),

    TERM = alcove:signal_define(Port, 'SIGTERM'),

    SA0 = alcove:sigaction(Port, [Child1], TERM, ign),
    Kill0 = alcove:kill(Port, Child1, TERM),
    Pid0 = alcove:getpid(Port, [Child1]),

    SA1 = alcove:sigaction(Port, [Child1], TERM, dfl),
    Kill1 = alcove:kill(Port, Child1, TERM),
    waitpid(Port, [], Child1),
    alcove:kill(Port, Child1, 0),
    Search = alcove:kill(Port, Child1, 0),

    [
        ?_assertEqual(ok, SA0),
        ?_assertEqual(ok, Kill0),
        ?_assertEqual(true, is_integer(Pid0)),

        ?_assertEqual(ok, SA1),
        ?_assertEqual(ok, Kill1),
        ?_assertEqual({error,esrch}, Search)
    ].

portstress(#state{port = Port, child = Child}) ->
    Reply = [ alcove:version(Port, [Child]) || _ <- lists:seq(1,1000) ],
    Ok = lists:filter(fun
            (false) -> false;
            (_) -> true
            end, Reply),
    ?_assertEqual(Ok, Reply).

forkstress(#state{port = Port}) ->
    {ok, Fork} = alcove:fork(Port),
    Reply = forkstress_1(Port, Fork, 100),
    ?_assertEqual(ok, Reply).

forkchain(#state{port = Port}) ->
    {ok, Child0} = alcove:fork(Port),
    {ok, Child1} = alcove:fork(Port, [Child0]),
    {ok, Child2} = alcove:fork(Port, [Child0, Child1]),
    {ok, Child3} = alcove:fork(Port, [Child0, Child1, Child2]),
    {ok, Child4} = alcove:fork(Port, [Child0, Child1, Child2, Child3]),

    Pid = alcove:getpid(Port, [Child0, Child1, Child2, Child3, Child4]),

    ?_assertEqual(Pid, Child4).

prctl(#state{os = {unix,linux}, port = Port}) ->
    {ok, Fork} = alcove:fork(Port),

    % capability is set:
    %   returns 0 | 1 in function result, arg2 = int
    PR_CAPBSET_READ = alcove:prctl_define(Port, 'PR_CAPBSET_READ'),
    Reply0 = alcove:prctl(Port, [Fork], PR_CAPBSET_READ, 0, 0,0,0),

    % set process name:
    %   arg2 = char *, up to 16 bytes, NULL terminated
    PR_SET_NAME = alcove:prctl_define(Port, 'PR_SET_NAME'),
    Reply1 = alcove:prctl(Port, [Fork], PR_SET_NAME, <<"test",0>>, 0,0,0),

    % get process name
    %   value returned in arg2 = char *, up to 16 bytes
    PR_GET_NAME = alcove:prctl_define(Port, 'PR_GET_NAME'),
    Reply2 = alcove:prctl(Port, [Fork], PR_GET_NAME, <<0:(17*8)>>, 0,0,0),

    % set parent death signal
    %  arg2 = signal
    PR_SET_PDEATHSIG = alcove:prctl_define(Port, 'PR_SET_PDEATHSIG'),
    Reply3 = alcove:prctl(Port, [Fork], PR_SET_PDEATHSIG, 9, 0,0,0),

    % get parent death signal
    %  arg2 = int *
    PR_GET_PDEATHSIG = alcove:prctl_define(Port, 'PR_GET_PDEATHSIG'),
    Reply4 = alcove:prctl(Port, [Fork], PR_GET_PDEATHSIG, <<0:32>>, 0,0,0),

    [
        ?_assertEqual({ok,1,0,0,0,0}, Reply0),
        ?_assertEqual({ok,0,<<116,101,115,116,0>>,0,0,0}, Reply1),
        ?_assertMatch({ok,0,<<116,101,115,116,0,0,0,0,0,0,0,0,0,0,0,0,0>>,0,0,0}, Reply2),
        ?_assertMatch({ok,0,9,0,0,0}, Reply3),
        ?_assertMatch({ok,0,<<9,0,0,0>>,0,0,0}, Reply4)
    ];
prctl(_) ->
    ?_assertEqual(ok,ok).


execvp(#state{os = {unix,linux}, port = Port, child = Child}) ->
    % cwd = /, chroot'ed in /bin
    Reply = alcove:execvp(Port, [Child], "/busybox", ["/busybox", "sh"]),
    ?_assertEqual(ok, Reply);
execvp(#state{os = {unix,freebsd}, port = Port, child = Child}) ->
    % cwd = /, chroot'ed in /rescue
    Reply = alcove:execvp(Port, [Child], "/sh", ["/sh"]),
    ?_assertEqual(ok, Reply).

stdout(#state{port = Port, child = Child}) ->
    Reply = alcove:stdin(Port, [Child], "echo 0123456789\n"),
    Stdout = alcove:stdout(Port, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertEqual(<<"0123456789\n">>, Stdout)
    ].

stderr(#state{os = {unix,linux}, port = Port, child = Child}) ->
    Reply = alcove:stdin(Port, [Child], "nonexistent 0123456789\n"),
    Stderr = alcove:stderr(Port, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertMatch(<<"sh: ", _/binary>>, Stderr)
    ];
stderr(#state{os = {unix,freebsd}, port = Port, child = Child}) ->
    Reply = alcove:stdin(Port, [Child], "nonexistent 0123456789\n"),
    Stderr = alcove:stderr(Port, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertEqual(<<"nonexistent: not found\n">>, Stderr)
    ].

execve(#state{port = Port}) ->
    {ok, Child0} = alcove:fork(Port),
    {ok, Child1} = alcove:fork(Port),

    Reply0 = alcove:execve(Port, [Child0], "/usr/bin/env",
        ["/usr/bin/env"], ["FOO=bar", "BAR=1234567"]),
    Reply1 = alcove:execve(Port, [Child1], "/usr/bin/env",
        ["/usr/bin/env"], []),

    Stdout0 = alcove:stdout(Port, [Child0], 5000),
    Stdout1 = alcove:stdout(Port, [Child1], 5000),

    [
        ?_assertEqual(ok, Reply0),
        ?_assertEqual(ok, Reply1),

        ?_assertEqual(<<"FOO=bar\nBAR=1234567\n">>, Stdout0),
        ?_assertEqual(false, Stdout1)
    ].

%%
%% Utility functions
%%
waitpid(Port, Pids, Child) ->
    SIGCHLD = alcove:signal_define(Port, 'SIGCHLD'),
    case alcove:event(Port, Pids, 5000) of
        {signal, SIGCHLD} ->
            waitpid_1(Port, Pids, Child);
        false ->
            false
    end.

waitpid_1(Port, Pids, Child) ->
    case alcove:kill(Port, Pids, Child, 0) of
        ok ->
            timer:sleep(10),
            waitpid_1(Port, Pids, Child);
        {error,esrch} ->
            ok
    end.

flush(stdout, Port, Pids) ->
    case alcove:stdout(Port, Pids) of
        false ->
            ok;
        _ ->
            flush(stdout, Port, Pids)
    end.

forkstress_1(_Port, _Child, 0) ->
    ok;
forkstress_1(Port, Child, N) ->
    {ok, Fork} = alcove:fork(Port, [Child]),
    ok = alcove:exit(Port, [Child,Fork], 0),
    case alcove:version(Port, [Child]) of
        {error, timedout} ->
            fail;
        _ ->
            forkstress_1(Port, Child, N-1)
    end.
