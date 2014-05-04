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
-module(tcplxc).
-export([start/0, start/1]).
-include_lib("alcove/include/alcove.hrl").

start() ->
    start([]).

start(Options) ->
    Init = spawn_link(fun() -> init(Options) end),
    Port = proplists:get_value(port, Options, 31337),
    {ok, LSock} = gen_tcp:listen(Port, [
            binary,
            {active,false},
            {reuseaddr,true}
        ]),
    accept(Init, LSock).

init(Options) ->
    {ok, Drv} = alcove_drv:start(Options ++ [{exec, "sudo"}]),
    ok = alcove:chdir(Drv, "/"),

    chroot_init(),
    cgroup_init(Drv),

    shell(Drv, Options, dict:new()).

shell(Drv, Options, State) ->
    receive
        {create, Pid} ->
            case clone(Drv, Options) of
                {ok, Child} ->
                    case catch clone_init(Drv, Child, Options) of
                        ok ->
                            Pid ! {ok, Child},
                            erlang:monitor(process, Pid),
                            shell(Drv, Options, dict:store(Child, Pid, State));
                        Error ->
                            Pid ! Error,
                            alcove:exit(Drv, [Child], 0),
                            shell(Drv, Options, State)
                    end;
                Error ->
                    Pid ! Error,
                    shell(Drv, Options, State)
            end;
        {'DOWN', _MonitorRef, _Type, Pid, _Info} ->
            Proc = [ K || {K, P} <- dict:to_list(State), P =:= Pid ],
            case Proc of
                [] -> ok;
                [Child] ->
                    alcove:kill(Drv, Child, 9),
                    shell(Drv, Options, dict:erase(Child, State))
            end;
        {stdin, Child, Data} ->
            error_logger:info_report([
                    {child, Child},
                    {stdin, Data}
                ]),
            alcove:stdin(Drv, [Child], Data),
            shell(Drv, Options, State);
        {alcove_stdout, Drv, [Child], Data} ->
            case dict:find(Child, State) of
                error ->
                    error_logger:error_report([
                            {child, Child},
                            {stdout, Data}
                        ]);
                {ok, Pid} ->
                    Pid ! {stdout, Child, Data}
            end,
            shell(Drv, Options, State);
        {alcove_stderr, Drv, [Child], Data} ->
            case dict:find(Child, State) of
                error ->
                    error_logger:error_report([
                            {child, Child},
                            {stderr, Data}
                        ]);
                {ok, Pid} ->
                    Pid ! {stderr, Child, Data}
            end,
            shell(Drv, Options, State)
    end.

clone(Drv, _Options) ->
    Flags = alcove:define(Drv, [
            'CLONE_NEWIPC',
            'CLONE_NEWNET',
            'CLONE_NEWNS',
            'CLONE_NEWPID',
            'CLONE_NEWUTS'
        ]),
    alcove:clone(Drv, Flags).

clone_init(Drv, Child, _Options) ->
    case alcove_cgroup:supported(Drv) of
        true ->
            {ok,_} = alcove_cgroup:set(Drv, [], <<>>, [<<"alcove">>],
                <<"tasks">>, integer_to_list(Child));
        false ->
            ok
    end,

    Id = id(),

    Hostname = lists:concat(["alcove", Id]),
    ok = alcove:sethostname(Drv, [Child], Hostname),

    BindFlags= alcove:define(Drv, [
            'MS_BIND',
            'MS_RDONLY',
            'MS_NOSUID'
        ]),

    HomeFlags= alcove:define(Drv, [
            'MS_NODEV',
            'MS_NOATIME',
            'MS_NOSUID'
        ]),

    ok = alcove:mount(Drv, [Child], "/bin",
        "/tmp/tcplxc/bin", "", BindFlags, <<>>),

    ok = alcove:mount(Drv, [Child], "/dev",
        "/tmp/tcplxc/dev", "", BindFlags, <<>>),

    ok = alcove:mount(Drv, [Child], "tmpfs",
        "/tmp/tcplxc/home", "tmpfs", HomeFlags,
        [<<"uid=">>, integer_to_binary(Id),
         <<",gid=">>, integer_to_binary(Id),
         <<",mode=700,size=16M">>]),

    % proc on /proc type proc (rw,noexec,nosuid,nodev)
    ProcFlags= alcove:define(Drv, [
            'MS_NOEXEC',
            'MS_NOSUID',
            'MS_NODEV'
        ]),
    ok = alcove:mount(Drv, [Child], "proc",
        "/proc", "proc", ProcFlags, <<>>),

    [ alcove:umount(Drv, [Child], Dir) || Dir <- mounts(),
        Dir =/= <<"/">>,
        Dir =/= <<"/bin">>,
        Dir =/= <<"/home">>,
        Dir =/= <<"/dev">>,
        Dir =/= <<"/proc">>
    ],

    alcove:umount(Drv, [Child], "/proc"),

    ok = alcove:chroot(Drv, [Child], "/tmp/tcplxc"),
    ok = alcove:chdir(Drv, [Child], "/"),

    ok = alcove:mount(Drv, [Child], "proc",
        "/proc", "proc", ProcFlags, <<>>),

    ok = alcove:setgid(Drv, [Child], Id),
    ok = alcove:setuid(Drv, [Child], Id),

    ok = alcove:chdir(Drv, [Child], "/home"),

    RLIMIT_NPROC = alcove:rlimit_define(Drv, 'RLIMIT_NPROC'),

    ok = alcove:setrlimit(Drv, [Child], RLIMIT_NPROC,
                          #rlimit{cur = 16, max = 16}),

    ok = alcove:execve(Drv, [Child], "/bin/busybox",
        ["/bin/busybox", "sh", "-i"],
        [
            "CONTAINER=alcove",
            "HOME=/home",
            "HOSTNAME=" ++ Hostname
        ]
    ),
    ok.

accept(Init, LSock) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    Pid = spawn(fun() -> network(Init, Socket) end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    accept(Init, LSock).

network(Init, Socket) ->
    Init ! {create, self()},
    receive
        {ok, Child} ->
            network(Init, Socket, Child);
        Error ->
            Data = io_lib:format("~p", [Error]),
            gen_tcp:send(Socket, Data)
    after
        10000 ->
            gen_tcp:send(Socket, "timeout")
    end.

network(Init, Socket, Child) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            Init ! {stdin, Child, Data},
            network(Init, Socket, Child);
        {tcp_closed, Socket} ->
            error_logger:error_report([{socket, closed}]),
            ok;
        {tcp_error, Socket, Error} ->
            error_logger:error_report([{socket, Error}]),
            ok;
        {stdout, Child, Data} ->
            ok = gen_tcp:send(Socket, Data),
            network(Init, Socket, Child);
        {stderr, Child, Data} ->
            ok = gen_tcp:send(Socket, Data),
            network(Init, Socket, Child);
        Any ->
            error_logger:info_report([
                    {init, Init},
                    {child, Child},
                    {unmatched, Any}
                ])
    end.

id() ->
    crypto:rand_uniform(16#f0000000, 16#f000ffff).

mounts() ->
    {ok, FH} = file:open("/proc/mounts", [read,raw,binary]),
    mountdir(FH).

mountdir(FH) ->
    mountdir(FH, []).

mountdir(FH, Acc) ->
    case file:read_line(FH) of
        eof ->
            lists:reverse(lists:sort(Acc));
        {ok, Data} ->
            Line = binary:split(Data, [<<"\s">>], [global]),
            Mount = lists:nth(2, Line),
            mountdir(FH, [Mount|Acc]);
        Error ->
            Error
    end.

chroot_init() ->
    [ ok = filelib:ensure_dir(lists:concat([Dir, "/."])) || Dir <- [
            "/tmp/tcplxc/bin",
            "/tmp/tcplxc/dev",
            "/tmp/tcplxc/home",
            "/tmp/tcplxc/lib",
            "/tmp/tcplxc/proc",
            "/tmp/tcplxc/sbin",
            "/tmp/tcplxc/usr/bin",
            "/tmp/tcplxc/usr/sbin"
        ] ],
    ok.

cgroup_init(Drv) ->
    case alcove_cgroup:supported(Drv) of
        true ->
            [ok] = alcove_cgroup:create(Drv, [], [<<"alcove">>]),
            alcove_cgroup:set(Drv, [], <<"cpuset">>, [<<"alcove">>],
                <<"cpuset.cpus">>, <<"0">>),
            alcove_cgroup:set(Drv, [], <<"cpuset">>, [<<"alcove">>],
                <<"cpuset.mems">>, <<"0">>),
            alcove_cgroup:set(Drv, [], <<"memory">>, [<<"alcove">>],
                <<"memory.memsw.limit_in_bytes">>, <<"16m">>),
            alcove_cgroup:set(Drv, [], <<"memory">>, [<<"alcove">>],
                <<"memory.limit_in_bytes">>, <<"16m">>);
        false ->
            ok
    end.
