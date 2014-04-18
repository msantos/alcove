%%% Copyright (c) 2014, Michael Santos <michael.santos@gmail.com>
%%%
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-module(nsex).
-include_lib("alcove/include/alcove.hrl").

-export([
        start/0,
        sandbox/1, sandbox/2
    ]).

start() ->
    Port = alcove_drv:start([{exec, "sudo"}]),
    case alcove_cgroup:supported(Port) of
        true ->
            ok = alcove_cgroup:create(Port, [], <<"alcove">>),
            {ok,1} = alcove_cgroup:set(Port, [], <<"cpuset">>, <<"alcove">>,
                <<"cpuset.cpus">>, <<"0">>),
            {ok,1} = alcove_cgroup:set(Port, [], <<"cpuset">>, <<"alcove">>,
                <<"cpuset.mems">>, <<"0">>),
            alcove_cgroup:set(Port, [], <<"memory">>, <<"alcove">>,
                <<"memory.memsw.limit_in_bytes">>, <<"16m">>),
            {ok,3} = alcove_cgroup:set(Port, [], <<"memory">>, <<"alcove">>,
                <<"memory.limit_in_bytes">>, <<"16m">>),
            Port;
        false ->
            alcove_drv:stop(Port),
            {error,unsupported}
    end.

sandbox(Port) ->
    sandbox(Port, ["/bin/busybox", "cat"]).
sandbox(Port, Argv) ->
    {Path, Arg0, Args} = argv(Argv),

    Flags = alcove:define(Port, [
            'CLONE_NEWIPC',
            'CLONE_NEWNET',
            'CLONE_NEWNS',
            'CLONE_NEWPID',
            'CLONE_NEWUTS'
        ]),
    {ok, Child} = alcove:clone(Port, Flags),

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
    {ok,_} = alcove_cgroup:set(Port, [], <<>>, <<"alcove">>,
        <<"tasks">>, integer_to_list(Child)).

chroot(Port, Child, Path) ->
    ok = alcove:chroot(Port, [Child], Path),
    ok = alcove:chdir(Port, [Child], "/").

drop_privs(Port, Child, Id) ->
    ok = alcove:setgid(Port, [Child], Id),
    ok = alcove:setuid(Port, [Child], Id).

id() ->
    16#f0000000 + crypto:rand_uniform(0, 16#ffff).
