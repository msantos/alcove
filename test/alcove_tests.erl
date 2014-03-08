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

run({Port, Child}) ->
    % Test order must be maintained
    [
        version(Port),
        pid(Port),
        getpid(Port, Child),
        sethostname(Port, Child),
        chroot(Port, Child),
        chdir(Port, Child),
        setrlimit(Port, Child),
        setgid(Port, Child),
        setuid(Port, Child),
        execvp(Port, Child),
        stdin(Port, Child)
    ].

-define(CLONE_NEWPID, 16#20000000).
-define(CLONE_NEWUTS, 16#04000000).

start() ->
    Port = alcove_drv:start([{exec, "sudo"}]),
    {ok, Child} = case os:type() of
        {unix,linux} ->
            alcove:clone(Port, ?CLONE_NEWPID bxor ?CLONE_NEWUTS);
        {unix,_} ->
            alcove:fork(Port)
    end,
    {Port, Child}.

stop({Port, _Child}) ->
    alcove_drv:stop(Port).

version(Port) ->
    Version = alcove:version(Port),
    ?_assertEqual(true, is_binary(Version)).

pid(Port) ->
    Pids = alcove:pid(Port),
    ?_assertEqual(1, length(Pids)).

getpid(Port, Child) ->
    PID = alcove:getpid(Port, [Child]),
    case os:type() of
        {unix,linux} ->
            % Running in a PID namespace
            ?_assertEqual(1, PID);
        {unix,_} ->
            ?_assertEqual(true, is_integer(PID))
    end.

sethostname(Port, Child) ->
    Reply = case os:type() of
        {unix,linux} ->
            alcove:sethostname(Port, [Child], "alcove");
        {unix,_} ->
            ok
    end,
    Hostname = alcove:gethostname(Port, [Child]),
    case os:type() of
        {unix,linux} ->
            [?_assertEqual(ok, Reply),
                ?_assertEqual({ok, <<"alcove">>}, Hostname)];
        {unix,_} ->
            [?_assertEqual(ok, Reply),
                ?_assertMatch({ok, <<_/binary>>}, Hostname)]
    end.

chroot(Port, Child) ->
    Reply = alcove:chroot(Port, [Child], "/bin"),
    ?_assertEqual(ok, Reply).

chdir(Port, Child) ->
    Reply = alcove:chdir(Port, [Child], "/"),
    CWD = alcove:getcwd(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"/">>}, CWD)
    ].

-define(RLIMIT_NOFILE, 7).

setrlimit(Port, Child) ->
    Reply = alcove:setrlimit(Port, [Child], ?RLIMIT_NOFILE, #rlimit{cur = 64, max = 64}),
    Rlimit = alcove:getrlimit(Port, [Child], ?RLIMIT_NOFILE),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, #rlimit{cur = 64, max = 64}}, Rlimit)
    ].

setgid(Port, Child) ->
    Reply = alcove:setgid(Port, [Child], 65534),
    GID = alcove:getgid(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, GID)
    ].

setuid(Port, Child) ->
    Reply = alcove:setuid(Port, [Child], 65534),
    UID = alcove:getuid(Port, [Child]),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, UID)
    ].

execvp(Port, Child) ->
    % cwd = /, chroot'ed in /bin
    Reply = alcove:execvp(Port, [Child], "/busybox", ["/busybox", "sh", "-i"]),
    Stderr = alcove:stderr(Port, [Child], 5000),
    [
        ?_assertEqual(ok, Reply),
        ?_assertNotEqual(false, Stderr)
    ].

stdin(Port, Child) ->
    Reply = alcove:stdin(Port, [Child], "help\n"),
    Stdout = alcove:stdout(Port, [Child], 5000),
    [
        ?_assertEqual(true, Reply),
        ?_assertNotEqual(false, Stdout)
    ].
