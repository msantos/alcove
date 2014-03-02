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

erlxc_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun run/1
    }.

run(Port) ->
    [
        version(Port),
        chroot(Port),
        chdir(Port),
        setrlimit(Port),
        setgid(Port),
        setuid(Port)
    ].

start() ->
    alcove_drv:start().

stop(Port) ->
    alcove_drv:stop(Port).

version(Port) ->
    Version = alcove:version(Port),
    ?_assertEqual(true, is_binary(Version)).

chroot(Port) ->
    Reply = alcove:chroot(Port, "/bin"),
    ?_assertEqual(ok, Reply).

chdir(Port) ->
    Reply = alcove:chdir(Port, "/"),
    CWD = alcove:getcwd(Port),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, <<"/">>}, CWD)
    ].

setrlimit(Port) ->
    Reply = alcove:setrlimit(Port, 7, #rlimit{cur = 64, max = 64}),
    Rlimit = alcove:getrlimit(Port, 7),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual({ok, #rlimit{cur = 64, max = 64}}, Rlimit)
    ].

setgid(Port) ->
    Reply = alcove:setgid(Port, 65534),
    GID = alcove:getgid(Port),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, GID)
    ].

setuid(Port) ->
    Reply = alcove:setuid(Port, 65534),
    UID = alcove:getuid(Port),
    [
        ?_assertEqual(ok, Reply),
        ?_assertEqual(65534, UID)
    ].
