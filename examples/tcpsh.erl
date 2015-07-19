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
-module(tcpsh).
-export([start/0, start/1]).

%% Bind a shell, running in a namespace, to a port:
%%
%%  $ erl -s tcpsh start
%%
%% In another shell:
%%
%%  $ nc localhost 31337
%%
start() ->
    start([]).

start(Options) ->
    TCPPort = proplists:get_value(port, Options, 31337),
    {ok, LSock} = gen_tcp:listen(TCPPort, [
            binary,
            {active,false},
            {reuseaddr,true}
        ]),
    accept(LSock, Options).

accept(LSock, Options) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    Pid = spawn(fun() -> create(Socket, Options) end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    accept(LSock, Options).

create(Socket, _Options) ->
    {ok, Drv} = alcove_drv:start_link([{exec,"sudo"}]),

    ok = alcove:chdir(Drv, [], "/"),

    {ok, PID} = alcove:clone(Drv, [], [
            clone_newipc,
            clone_newnet,
            clone_newns,
            clone_newpid,
            clone_newuts
        ]),

    Id = id(),

    ok = alcove:sethostname(Drv, [PID], ["alcove", integer_to_list(Id)]),

    % proc on /proc type proc (rw,noexec,nosuid,nodev)
    MountFlags= [
        ms_noexec,
        ms_nosuid,
        ms_nodev
    ],
    ok = alcove:mount(Drv, [PID], "proc", "/proc", "proc", MountFlags, <<>>, <<>>),

    ok = alcove:setgid(Drv, [PID], Id),
    ok = alcove:setuid(Drv, [PID], Id),
    ok = alcove:execve(Drv, [PID], "/bin/busybox",
        ["/bin/busybox", "sh", "-i"], ["HOME=/"]),
    shell(Drv, Socket, PID).

shell(Drv, Socket, PID) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            alcove:stdin(Drv, [PID], Data),
            shell(Drv, Socket, PID);
        {tcp_closed, Socket} ->
            error_logger:error_report([{socket, closed}]),
            ok;
        {tcp_error, Socket, Error} ->
            error_logger:error_report([{socket, Error}]),
            ok;
        {alcove_stdout, Drv, [PID], Data} ->
            ok = gen_tcp:send(Socket, Data),
            shell(Drv, Socket, PID);
        {alcove_stderr, Drv, [PID], Data} ->
            ok = gen_tcp:send(Socket, Data),
            shell(Drv, Socket, PID);
        Any ->
            error_logger:info_report([{drv, Drv}, {pid, PID}, {any, Any}])
    end.

id() ->
    crypto:rand_uniform(16#f0000000, 16#f000ffff).
