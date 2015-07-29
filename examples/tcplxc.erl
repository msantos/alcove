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
-include_lib("alcove/include/alcove.hrl").

-export([start/0, start/1]).

start() ->
    start([]).

start(Options) ->
    {ok, Drv} = alcove_drv:start_link(Options ++ [{exec, "sudo"}]),

    ok = alcove:chdir(Drv, [], "/"),
    chroot_init(),
    cgroup_init(Drv,
        [<<"alcove">>],
        Options ++ [
            {<<"memory.memsw.limit_in_bytes">>, <<"128m">>},
            {<<"memory.limit_in_bytes">>, <<"128m">>}
        ]),

    Port = proplists:get_value(port, Options, 31337),
    {ok, LSock} = gen_tcp:listen(Port, [
            binary,
            {active,false},
            {reuseaddr,true}
        ]),
    accept(Drv, LSock, Options).

accept(Drv, LSock, Options) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    Pid = spawn(fun() -> connection(Drv, Socket, Options) end),
    ok = gen_tcp:controlling_process(Socket, Pid),
    accept(Drv, LSock, Options).

connection(Drv, Socket, Options) ->
    case clone(Drv, Options) of
        {ok, Child} ->
            case catch clone_init(Drv, Child, Options) of
                ok ->
                    network(Drv, Socket, Child, Options);
                Error ->
                    alcove:exit(Drv, [Child], 0),
                    Data = io_lib:format("~p", [Error]),
                    gen_tcp:send(Socket, Data)
            end;
        Error ->
            Data = io_lib:format("~p", [Error]),
            gen_tcp:send(Socket, Data)
    end.

network(Drv, Socket, Child, Options) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            error_logger:info_report([
                    {peer, element(2, inet:peername(Socket))},
                    {child, Child},
                    {stdin, Data}
                ]),
            alcove:stdin(Drv, [Child], Data),
            network(Drv, Socket, Child, Options);
        {tcp_closed, Socket} ->
            error_logger:error_report([{socket, closed}]),
            alcove:exit(Drv, [Child], 0);
        {tcp_error, Socket, Error} ->
            error_logger:error_report([{socket, Error}]),
            alcove:exit(Drv, [Child], 0);

        {alcove_stdout, Drv, [Child], Data} ->
            ok = gen_tcp:send(Socket, Data),
            network(Drv, Socket, Child, Options);
        {alcove_stderr, Drv, [Child], Data} ->
            ok = gen_tcp:send(Socket, Data),
            network(Drv, Socket, Child, Options);
        {alcove_event, Drv, [Child], {termsig,_} = Event} ->
            error_logger:info_report([{pid, Child}, Event]),
            cgroup_finish(Drv, Child, Options),
            network_drain(Drv, Socket, Child);
        {alcove_event, Drv, [Child], {exit_status,_} = Event} ->
            error_logger:info_report([{pid, Child}, Event]),
            cgroup_finish(Drv, Child, Options),
            network_drain(Drv, Socket, Child);

        Any ->
            error_logger:info_report([
                    {init, Drv},
                    {child, Child},
                    {unmatched, Any}
                ])
    end.

network_drain(Drv, Socket, Child) ->
    receive
        {tcp, Socket, _Data} ->
            network_drain(Drv, Socket, Child);
        {tcp_closed, Socket} ->
            ok;
        {tcp_error, Socket, Error} ->
            error_logger:error_report([{socket, Error}]),
            ok;

        {alcove_stdout, Drv, [Child], Data} ->
            ok = gen_tcp:send(Socket, Data),
            network_drain(Drv, Socket, Child);
        {alcove_stderr, Drv, [Child], Data} ->
            ok = gen_tcp:send(Socket, Data),
            network_drain(Drv, Socket, Child);

        Any ->
            error_logger:info_report([
                    {child, Child},
                    {unmatched, Any}
                ])
    after
        10 ->
            ok
    end.

clone(Drv, _Options) ->
    alcove:clone(Drv, [], [
            clone_newipc,
            clone_newnet,
            clone_newns,
            clone_newpid,
            clone_newuts
        ]).

clone_init(Drv, Child, Options) ->
    Id = id(),
    Hostname = lists:concat(["alcove", Child]),

    case alcove_cgroup:supported(Drv, []) of
        true ->
            cgroup_init(Drv, [<<"alcove">>, Hostname], Options),
            ok = alcove_cgroup:set(Drv, [], <<>>, [<<"alcove">>, Hostname],
                <<"tasks">>, integer_to_list(Child));
        false ->
            ok
    end,

    ok = alcove:sethostname(Drv, [Child], Hostname),

    [ ok = bindmount(Drv, [Child], Src, "/tmp/tcplxc") || Src <- [
            "/lib",
            "/lib64",
            "/sbin",
            "/bin",
            "/usr",
            "/dev" ] ],

    ok = mount(Drv, [Child], "tmpfs", "/tmp/tcplxc/etc", "tmpfs", [
            ms_nodev,
            ms_noatime,
            ms_nosuid,
            ms_private
        ], [<<"mode=755,size=16M">>]),

    ok = mount(Drv, [Child], "tmpfs",
        "/tmp/tcplxc/home", "tmpfs", [
            ms_nodev,
            ms_noatime,
            ms_nosuid,
            ms_private
        ], [<<"uid=">>, integer_to_binary(Id),
         <<",gid=">>, integer_to_binary(Id),
         <<",mode=700,size=16M">>]),

    % proc on /proc type proc (rw,noexec,nosuid,nodev)
    ok = mount(Drv, [Child], "proc",
        "/proc", "proc", [
            ms_noexec,
            ms_nosuid,
            ms_nodev,
            ms_private
        ], <<>>),

    [ alcove:umount(Drv, [Child], Dir) || Dir <- mounts(),
        Dir =/= <<"/">>,
        Dir =/= <<"/lib">>,
        Dir =/= <<"/bin">>,
        Dir =/= <<"/usr">>,
        Dir =/= <<"/home">>,
        Dir =/= <<"/dev">>,
        Dir =/= <<"/proc">>
    ],

    alcove:umount(Drv, [Child], "/proc"),

    ok = alcove:chroot(Drv, [Child], "/tmp/tcplxc"),
    ok = alcove:chdir(Drv, [Child], "/"),

    % devpts on /dev/pts type devpts (rw,noexec,nosuid,gid=5,mode=0620)
    ok = mount(Drv, [Child], "devpts",
        "/dev/pts", "devpts", [ms_noexec, ms_nosuid, ms_private],
        [<<"mode=620,gid=5">>]),

    ok = mount(Drv, [Child], "proc",
        "/proc", "proc", [
            ms_noexec,
            ms_nosuid,
            ms_nodev,
            ms_private
        ], <<>>),

    SysFiles = proplists:get_value(system_files, Options, []),
    write_files(Drv, [Child], [
        {"/etc/passwd", lists:concat(["root:x:0:0:root:/root:/bin/bash
alcove:x:", Id, ":", Id, ":root:/root:/bin/bash"])},
        {"/etc/group", lists:concat(["root:x:0:
tty:x:5:
alcove:x:", Id, ":"])}
    ] ++ SysFiles),

    ok = alcove:setgid(Drv, [Child], Id),
    ok = alcove:setuid(Drv, [Child], Id),

    ok = alcove:chdir(Drv, [Child], "/home"),

    Files = proplists:get_value(files, Options, []),
    write_files(Drv, [Child], Files),

    ok = alcove:setrlimit(Drv, [Child], rlimit_nproc,
                          #alcove_rlimit{cur = 16, max = 16}),

    Exe = proplists:get_value(exe, Options, ["/bin/bash", "-i"]),
    Env = proplists:get_value(environ, Options, []),

    ok = alcove:setpriority(Drv, [Child], prio_process, 0, 20),

    ok = alcove:execve(Drv, [Child], hd(Exe), Exe, [
        "PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin",
        "TERM=linux",
        "CONTAINER=alcove",
        "HOME=/home",
        "TMPDIR=/home",
        "HOSTNAME=" ++ Hostname
    ] ++ Env).

id() ->
    crypto:rand_uniform(16#f0000000, 16#f000ffff).

mount(Drv, Pids, Source, Target, Type, Flags, Data) ->
    alcove:mount(Drv, Pids, Source, Target, Type, Flags, Data, <<>>).

mounts() ->
    {ok, FH} = file:open("/proc/mounts", [read,raw,binary]),
    Reply = mountdir(FH),
    file:close(FH),
    Reply.

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

bindmount(Drv, Pids, Src, DstPath) ->
    case file:read_file_info(Src) of
        {error,enoent} ->
            ok;
        {ok, _} ->
            ok = mount(Drv, Pids, Src, join(DstPath, Src),
                "", [ms_bind], <<>>),
            mount(Drv, Pids, Src, join(DstPath, Src), "", [
                    ms_remount,
                    ms_bind,
                    ms_rdonly,
                    ms_nosuid,
                    ms_private
                ], <<>>)
    end.

chroot_init() ->
    [ ok = filelib:ensure_dir(lists:concat([Dir, "/."])) || Dir <- [
            "/tmp/tcplxc/etc",
            "/tmp/tcplxc/bin",
            "/tmp/tcplxc/dev",
            "/tmp/tcplxc/home",
            "/tmp/tcplxc/lib",
            "/tmp/tcplxc/lib64",
            "/tmp/tcplxc/proc",
            "/tmp/tcplxc/sbin",
            "/tmp/tcplxc/usr"
        ] ],
    ok.

cgroup_init(Drv, Namespace, Options) ->
    Cpus = proplists:get_value(<<"cpuset.cpus">>, Options, <<"0">>),
    Mems = proplists:get_value(<<"cpuset.mems">>, Options, <<"0">>),
    SWBytes = proplists:get_value(<<"memory.memsw.limit_in_bytes">>,
                                  Options, <<"16m">>),
    Bytes = proplists:get_value(<<"memory.limit_in_bytes">>,
                                Options, <<"16m">>),

    case alcove_cgroup:supported(Drv, []) of
        true ->
            alcove_cgroup:create(Drv, [], Namespace),
            alcove_cgroup:set(Drv, [], <<"cpuset">>, Namespace,
                <<"cpuset.cpus">>, Cpus),
            alcove_cgroup:set(Drv, [], <<"cpuset">>, Namespace,
                <<"cpuset.mems">>, Mems),
            alcove_cgroup:set(Drv, [], <<"memory">>, Namespace,
                <<"memory.memsw.limit_in_bytes">>, SWBytes),
            alcove_cgroup:set(Drv, [], <<"memory">>, Namespace,
                <<"memory.limit_in_bytes">>, Bytes);
        false ->
            ok
    end.

cgroup_finish(Drv, Child, _Options) ->
    Hostname = lists:concat(["alcove", Child]),
    alcove_cgroup:destroy(Drv, [], [<<"alcove">>, Hostname]).

join(Root, Path) ->
    P1 = filename:split(Root),
    P2 = case filename:split(Path) of
             ["/"|Rest] -> Rest;
             Rest -> Rest
         end,
    filename:join(P1 ++ P2).

write_files(_Drv, _Pid, []) ->
    ok;
write_files(Drv, Pid, [{Path, Contents, Mode}|Rest]) ->
    {ok, FD} = alcove:open(Drv, Pid, Path,
        [o_wronly, o_creat, o_cloexec], Mode),
    {ok, _} = alcove:write(Drv, Pid, FD, Contents),
    ok = alcove:close(Drv, Pid, FD),
    write_files(Drv, Pid, Rest);
write_files(Drv, Pid, [{Path, Contents}|Rest]) ->
    write_files(Drv, Pid, [{Path, Contents, 8#644}|Rest]).
