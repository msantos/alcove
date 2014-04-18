%%% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-module(chrootex).
-include_lib("alcove/include/alcove.hrl").

-export([
        start/0,
        sandbox/1, sandbox/2
    ]).

start() ->
    alcove_drv:start([{exec, "sudo"}]).

sandbox(Port) ->
    sandbox(Port, ["/bin/busybox", "cat"]).
sandbox(Port, Argv) ->
    {Path, Arg0, Args} = argv(Argv),

    {ok, Child} = alcove:fork(Port),

    setlimits(Port, Child),
    chroot(Port, Child, Path),
    drop_privs(Port, Child, id()),

    ok = alcove:execvp(Port, [Child], Arg0, [Arg0, Args]),

    Child.

argv([Arg0, Args]) ->
    Path = filename:dirname(Arg0),
    Progname = filename:join(["/", filename:basename(Arg0)]),
    {Path, Progname, Args}.

setlimits(Port, Child) ->
    RLIMIT_FSIZE = alcove:rlimit_define(Port, 'RLIMIT_FSIZE'),
    RLIMIT_NPROC = alcove:rlimit_define(Port, 'RLIMIT_NPROC'),
    RLIMIT_NOFILE = alcove:rlimit_define(Port, 'RLIMIT_NOFILE'),

    ok = alcove:setrlimit(Port, [Child], RLIMIT_FSIZE,
        #rlimit{cur = 0, max = 0}),

    ok = alcove:setrlimit(Port, [Child], RLIMIT_NPROC,
        #rlimit{cur = 1, max = 1}),

    ok = alcove:setrlimit(Port, [Child], RLIMIT_NOFILE,
        #rlimit{cur = 0, max = 0}).

chroot(Port, Child, Path) ->
    ok = alcove:chroot(Port, [Child], Path),
    ok = alcove:chdir(Port, [Child], "/").

drop_privs(Port, Child, Id) ->
    ok = alcove:setgid(Port, [Child], Id),
    ok = alcove:setuid(Port, [Child], Id).

id() ->
    16#f0000000 + crypto:rand_uniform(0, 16#ffff).
