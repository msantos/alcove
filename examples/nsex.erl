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
-module(nsex).
-include_lib("alcove/include/alcove.hrl").

-export([
        start/0,
        sandbox/1, sandbox/2
    ]).

start() ->
    {ok, Drv} = alcove_drv:start_link([{exec, "sudo"}]),
    case alcove_cgroup:supported(Drv, []) of
        true ->
            ok = alcove_cgroup:create(Drv, [], [<<"alcove">>]),
            ok = alcove_cgroup:set(Drv, [], <<"cpuset">>, [<<"alcove">>],
                <<"cpuset.cpus">>, <<"0">>),
            ok = alcove_cgroup:set(Drv, [], <<"cpuset">>, [<<"alcove">>],
                <<"cpuset.mems">>, <<"0">>),
            alcove_cgroup:set(Drv, [], <<"memory">>, [<<"alcove">>],
                <<"memory.memsw.limit_in_bytes">>, <<"16m">>),
            ok = alcove_cgroup:set(Drv, [], <<"memory">>, [<<"alcove">>],
                <<"memory.limit_in_bytes">>, <<"16m">>),
            Drv;
        false ->
            alcove_drv:stop(Drv),
            {error,unsupported}
    end.

sandbox(Drv) ->
    sandbox(Drv, ["/bin/busybox", "cat"]).
sandbox(Drv, Argv) ->
    {Path, Arg0, Args} = argv(Argv),

    {ok, Child} = alcove:clone(Drv, [], [
            clone_newipc,
            clone_newnet,
            clone_newns,
            clone_newpid,
            clone_newuts
        ]),

    setlimits(Drv, Child),
    chroot(Drv, Child, Path),
    drop_privs(Drv, Child, id()),

    ok = alcove:execvp(Drv, [Child], Arg0, [Arg0, Args]),

    Child.

argv([Arg0, Args]) ->
    Path = filename:dirname(Arg0),
    Progname = filename:join(["/", filename:basename(Arg0)]),
    {Path, Progname, Args}.

setlimits(Drv, Child) ->
    ok = alcove_cgroup:set(Drv, [], <<>>, [<<"alcove">>],
        <<"tasks">>, integer_to_list(Child)).

chroot(Drv, Child, Path) ->
    ok = alcove:chroot(Drv, [Child], Path),
    ok = alcove:chdir(Drv, [Child], "/").

drop_privs(Drv, Child, Id) ->
    ok = alcove:setgid(Drv, [Child], Id),
    ok = alcove:setuid(Drv, [Child], Id).

id() ->
    crypto:rand_uniform(16#f0000000, 16#f000ffff).
