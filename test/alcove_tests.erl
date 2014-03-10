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
        version(State),
        pid(State),
        getpid(State),
        sethostname(State),
        setns(State),
        unshare(State),
        mount(State),
        tmpfs(State),
        chroot(State),
        chdir(State),
        setrlimit(State),
        setgid(State),
        setuid(State),
        fork(State),
        execvp(State),
        stdin(State)
    ].

-define(CLONE_NEWPID, 16#20000000).
-define(CLONE_NEWUTS, 16#04000000).

start() ->
    Port = alcove_drv:start([{exec, "sudo"}]),
    case os:type() of
        {unix,linux} = OS ->
            Flags = alcove:define(Port, clone, [newns,newpid,newuts,newnet,newipc]),
            {ok, Child} = alcove:clone(Port, Flags),
            {OS, Port, Child};
        {unix,_} = OS ->
            {ok, Child} = alcove:fork(Port),
            {OS, Port, Child}
    end.

stop({_, Port, _Child}) ->
    alcove_drv:stop(Port).

version({_, Port, _Child}) ->
    Version = alcove:version(Port),
    ?_assertEqual(true, is_binary(Version)).

pid({_, Port, _Child}) ->
    Pids = alcove:pid(Port),
    ?_assertEqual(1, length(Pids)).

getpid({{unix,linux}, Port, Child}) ->
    % Running in a PID namespace
    PID = alcove:getpid(Port, [Child]),
    ?_assertEqual(1, PID);
getpid({_, Port, Child}) ->
    PID = alcove:getpid(Port, [Child]),
    ?_assertEqual(true, PID > 0).

sethostname({{unix,linux}, Port, Child}) ->
    Reply = alcove:sethostname(Port, [Child], "alcove"),
    Hostname = alcove:gethostname(Port, [Child]),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"alcove">>}, Hostname)];
sethostname({_, Port, Child}) ->
    Hostname = alcove:gethostname(Port, [Child]),
    ?_assertMatch({ok, <<_/binary>>}, Hostname).

setns({{unix,linux}, Port, Child}) ->
    {ok, Child1} = alcove:fork(Port),
    ok = alcove:setns(Port, [Child1], [
            "/proc/",
            integer_to_list(Child),
            "/ns/uts"
        ]),
    Hostname0 = alcove:gethostname(Port, [Child]),
    Hostname1 = alcove:gethostname(Port, [Child1]),
    ?_assertEqual(Hostname0, Hostname1);
setns({_, _Port, _Child}) ->
    ?_assertEqual(ok,ok).

unshare({{unix,linux}, Port, _Child}) ->
    {ok, Child1} = alcove:fork(Port),
    ok = alcove:unshare(Port, [Child1], alcove:clone_define(Port, newuts)),
    Reply = alcove:sethostname(Port, [Child1], "unshare"),
    Hostname = alcove:gethostname(Port, [Child1]),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"unshare">>}, Hostname)];
unshare({_, _Port, _Child}) ->
    ?_assertEqual(ok,ok).

mount({{unix,linux}, Port, Child}) ->
    Flags = alcove:define(Port, mount, [bind,rdonly,noexec]),
    Mount = alcove:mount(Port, [Child], "/tmp", "/mnt", "", Flags, ""),
    Umount = alcove:umount(Port, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
mount({_, _Port, _Child}) ->
    ?_assertEqual(ok,ok).

tmpfs({{unix,linux}, Port, Child}) ->
    Flags = alcove:define(Port, mount, [noexec]),
    Mount = alcove:mount(Port, [Child], "tmpfs", "/mnt", "tmpfs", Flags, <<"size=16M", 0>>),
    Umount = alcove:umount(Port, [Child], "/mnt"),
    [
        ?_assertEqual(ok, Mount),
        ?_assertEqual(ok, Umount)
    ];
tmpfs({_, _Port, _Child}) ->
    ?_assertEqual(ok,ok).

chroot({_, Port, Child}) ->
    Reply = alcove:chroot(Port, [Child], "/bin"),
    ?_assertEqual(ok, Reply).

chdir({_, Port, Child}) ->
    Reply = alcove:chdir(Port, [Child], "/"),
    CWD = alcove:getcwd(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"/">>}, CWD)
    ].

setrlimit({_, Port, Child}) ->
    RLIMIT_NOFILE = alcove:rlimit_define(Port, nofile),
    Reply = alcove:setrlimit(Port, [Child], RLIMIT_NOFILE, #rlimit{cur = 64, max = 64}),
    Rlimit = alcove:getrlimit(Port, [Child], RLIMIT_NOFILE),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, #rlimit{cur = 64, max = 64}}, Rlimit)
    ].

setgid({_, Port, Child}) ->
    Reply = alcove:setgid(Port, [Child], 65534),
    GID = alcove:getgid(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, GID)
    ].

setuid({_, Port, Child}) ->
    Reply = alcove:setuid(Port, [Child], 65534),
    UID = alcove:getuid(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, UID)
    ].

fork({_, Port, Child}) ->
    Pids = [ alcove:fork(Port, [Child]) || _ <- lists:seq(1, 32) ],
    [Last|_Rest] = lists:reverse(Pids),
    Reply = alcove:getpid(Port, [Child]),

    [
        ?_assertEqual(true, is_integer(Reply)),
        ?_assertEqual({error,eagain}, Last)
    ].

execvp({_, Port, Child}) ->
    % cwd = /, chroot'ed in /bin
    Reply = alcove:execvp(Port, [Child], "/busybox", ["/busybox", "sh", "-i"]),
    Stderr = alcove:stderr(Port, [Child], 5000),
    [
        ?_assertEqual(ok, Reply),
        ?_assertNotEqual(false, Stderr)
    ].

stdin({_, Port, Child}) ->
    Reply = alcove:stdin(Port, [Child], "help\n"),
    Stdout = alcove:stdout(Port, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertNotEqual(false, Stdout)
    ].
