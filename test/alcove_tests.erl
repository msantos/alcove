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
        {unix,linux} ->
            Flags = alcove:clone_flags(Port, newpid)
                bxor alcove:clone_flags(Port, newuts)
                bxor alcove:clone_flags(Port, newnet)
                bxor alcove:clone_flags(Port, newipc),
            {ok, Child} = alcove:clone(Port, Flags),
            {linux, Port, Child};
        {unix,_} ->
            {ok, Child} = alcove:fork(Port),
            {unix, Port, Child}
    end.

stop({_, Port, _Child}) ->
    alcove_drv:stop(Port).

version({_, Port, _Child}) ->
    Version = alcove:version(Port),
    ?_assertEqual(true, is_binary(Version)).

pid({_, Port, _Child}) ->
    Pids = alcove:pid(Port),
    ?_assertEqual(1, length(Pids)).

getpid({linux, Port, Child}) ->
    % Running in a PID namespace
    PID = alcove:getpid(Port, [Child]),
    ?_assertEqual(1, PID);
getpid({unix, Port, Child}) ->
    PID = alcove:getpid(Port, [Child]),
    ?_assertEqual(true, PID > 0).

sethostname({linux, Port, Child}) ->
    Reply = alcove:sethostname(Port, [Child], "alcove"),
    Hostname = alcove:gethostname(Port, [Child]),
    [?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"alcove">>}, Hostname)];
sethostname({unix, Port, Child}) ->
    Hostname = alcove:gethostname(Port, [Child]),
    ?_assertMatch({ok, <<_/binary>>}, Hostname).

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

-define(RLIMIT_NOFILE, 7).

setrlimit({_, Port, Child}) ->
    Reply = alcove:setrlimit(Port, [Child], ?RLIMIT_NOFILE, #rlimit{cur = 64, max = 64}),
    Rlimit = alcove:getrlimit(Port, [Child], ?RLIMIT_NOFILE),
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
