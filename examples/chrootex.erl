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
-module(chrootex).
-include_lib("alcove/include/alcove.hrl").

-export([
        start/0,
        sandbox/1, sandbox/2
    ]).

start() ->
    alcove_drv:start_link([{exec, "sudo"}]).

sandbox(Drv) ->
    sandbox(Drv, ["/bin/busybox", "cat"]).
sandbox(Drv, Argv) ->
    {Path, Arg0, Args} = argv(Argv),

    {ok, Child} = alcove:fork(Drv, []),

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
    ok = alcove:setrlimit(Drv, [Child], rlimit_fsize,
        #alcove_rlimit{cur = 0, max = 0}),

    ok = alcove:setrlimit(Drv, [Child], rlimit_nproc,
        #alcove_rlimit{cur = 1, max = 1}),

    ok = alcove:setrlimit(Drv, [Child], rlimit_nofile,
        #alcove_rlimit{cur = 0, max = 0}).

chroot(Drv, Child, Path) ->
    ok = alcove:chroot(Drv, [Child], Path),
    ok = alcove:chdir(Drv, [Child], "/").

drop_privs(Drv, Child, Id) ->
    ok = alcove:setgid(Drv, [Child], Id),
    ok = alcove:setuid(Drv, [Child], Id).

id() ->
    crypto:rand_uniform(16#f0000000, 16#f000ffff).
