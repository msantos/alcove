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
-module(alcove_seccomp_tests).

-compile(export_all).

-record(state, {
        pid,
        child,
        seccomp
    }).

-include_lib("eunit/include/eunit.hrl").
-include_lib("alcove/include/alcove.hrl").
-include_lib("alcove/include/alcove_seccomp.hrl").

alcove_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun run/1
    }.

run(#state{seccomp = true} = State) ->
    [
        kill(State),
        allow(State),
        trap(State)
    ];
run(_) ->
    [].

start() ->
    Exec = case os:getenv("ALCOVE_TEST_EXEC") of
        false -> "sudo";
        Env -> Env
    end,

    {ok, Drv} = alcove_drv:start([
            {exec, Exec},
            {maxchild, 8},
            termsig
        ]),

    #state{
        pid = Drv,
        seccomp = alcove:define(Drv, 'SECCOMP_MODE_FILTER') =/= false
    }.

stop(#state{pid = Drv}) ->
    alcove_drv:stop(Drv).

kill(#state{pid = Drv}) ->
    SIGSYS = alcove:define(Drv, 'SIGSYS'),

    {ok, Pid} = alcove:fork(Drv),
    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_KILL)),
    % Allowed: cached by process
    Reply0 = alcove:getpid(Drv, [Pid]),
    % Not allowed: SIGSYS
    alcove:getcwd(Drv, [Pid]),

    receive
        {alcove_event,Drv,[Pid],Event} ->
            ok
    end,

    Reply3 = alcove:kill(Drv, Pid, 0),

    [
        ?_assertEqual(Pid, Reply0),
        ?_assertEqual({termsig, SIGSYS}, Event),
        ?_assertEqual({error, esrch}, Reply3)
    ].

allow(#state{pid = Drv}) ->
    {ok, Pid} = alcove:fork(Drv),
    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_ALLOW)),
    Reply0 = alcove:getpid(Drv, [Pid]),
    Reply1 = alcove:getcwd(Drv, [Pid]),
    Reply2 = alcove:kill(Drv, Pid, 0),
    alcove:exit(Drv, [Pid], 0),

    [
        ?_assertEqual(Pid, Reply0),
        ?_assertMatch({ok,_}, Reply1),
        ?_assertEqual(ok, Reply2)
    ].

trap(#state{pid = Drv}) ->
    SIGSYS = alcove:define(Drv, 'SIGSYS'),

    {ok, Pid} = alcove:fork(Drv),
    ok = alcove:sigaction(Drv, [Pid], SIGSYS, trap),

    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_TRAP)),

    % Allowed: cached by process
    Reply0 = alcove:getpid(Drv, [Pid]),
    % Not allowed: SIGSYS
    Reply1 = case alcove:getcwd(Drv, [Pid]) of
        {error,unknown} -> true;
        {ok,<<>>} -> true;
        _ -> false
    end,

    receive
        {alcove_event,Drv,[Pid],Event} ->
            ok
    end,

    Reply3 = alcove:kill(Drv, Pid, 0),

    [
        ?_assertEqual(Pid, Reply0),
        ?_assertEqual(true, Reply1),
        ?_assertEqual({signal, SIGSYS}, Event),
        ?_assertEqual(ok, Reply3)
    ].

allow_syscall(Drv, Syscall) ->
    case alcove:define(Drv, Syscall) of
        false -> [];
        NR -> ?ALLOW_SYSCALL(NR)
    end.

filter(Drv) ->
    Arch = alcove:define(Drv, alcove:audit_arch()),
    [
        ?VALIDATE_ARCHITECTURE(Arch),
        ?EXAMINE_SYSCALL,
        allow_syscall(Drv, 'SYS_rt_sigreturn'),
        allow_syscall(Drv, 'SYS_sigreturn'),
        allow_syscall(Drv, 'SYS_exit_group'),
        allow_syscall(Drv, 'SYS_exit'),
        allow_syscall(Drv, 'SYS_read'),
        allow_syscall(Drv, 'SYS_write'),
        allow_syscall(Drv, 'SYS_writev'),
        allow_syscall(Drv, 'SYS_setrlimit'),
        allow_syscall(Drv, 'SYS_getrlimit'),
        allow_syscall(Drv, 'SYS_ugetrlimit'),
        allow_syscall(Drv, 'SYS_poll')
    ].

enforce(Drv, Pids, Filter0) ->
    Filter = filter(Drv) ++ [Filter0],
    PR_SET_NO_NEW_PRIVS = alcove:define(Drv, 'PR_SET_NO_NEW_PRIVS'),
    PR_SET_SECCOMP = alcove:define(Drv, 'PR_SET_SECCOMP'),
    SECCOMP_MODE_FILTER = alcove:define(Drv, 'SECCOMP_MODE_FILTER'),

    {ok,_,_,_,_,_} = alcove:prctl(Drv, Pids, PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0),

    Pad = (erlang:system_info({wordsize,external}) - 2) * 8,

    Prog = [
        <<(iolist_size(Filter) div 8):2/native-unsigned-integer-unit:8>>,
        <<0:Pad>>,
        {ptr, list_to_binary(Filter)}
    ],
    alcove:prctl(Drv, Pids,
        PR_SET_SECCOMP, SECCOMP_MODE_FILTER, Prog, 0, 0).
