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

    {ok, Drv} = alcove_drv:start_link([
            {exec, Exec},
            {maxchild, 8},
            termsig
        ]),

    #state{
        pid = Drv,
        seccomp = alcove:define(Drv, [], seccomp_mode_filter) =/= unknown
    }.

stop(#state{pid = Drv}) ->
    alcove_drv:stop(Drv).

kill(#state{pid = Drv}) ->
    {ok, Pid} = alcove:fork(Drv, []),
    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_KILL)),
    % Allowed: cached by process
    Reply0 = alcove:getpid(Drv, [Pid]),
    % Not allowed: SIGSYS
    Reply1 = (catch alcove:getcwd(Drv, [Pid])),

    Reply2 = alcove:kill(Drv, [], Pid, 0),

    [
        ?_assertEqual(Pid, Reply0),
        ?_assertMatch({'EXIT',{{termsig,sigsys},_}}, Reply1),
        ?_assertEqual({error, esrch}, Reply2)
    ].

allow(#state{pid = Drv}) ->
    {ok, Pid} = alcove:fork(Drv, []),
    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_ALLOW)),
    Reply0 = alcove:getpid(Drv, [Pid]),
    Reply1 = alcove:getcwd(Drv, [Pid]),
    Reply2 = alcove:kill(Drv, [], Pid, 0),
    alcove:exit(Drv, [Pid], 0),

    [
        ?_assertEqual(Pid, Reply0),
        ?_assertMatch({ok,_}, Reply1),
        ?_assertEqual(ok, Reply2)
    ].

trap(#state{pid = Drv}) ->
    {ok, Pid} = alcove:fork(Drv, []),
    ok = alcove:sigaction(Drv, [Pid], sigsys, sig_catch),

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

    Reply3 = alcove:kill(Drv, [], Pid, 0),
    alcove:exit(Drv, [Pid], 0),

    [
        ?_assertEqual(Pid, Reply0),
        ?_assertEqual(true, Reply1),
        ?_assertEqual({signal, sigsys}, Event),
        ?_assertEqual(ok, Reply3)
    ].

allow_syscall(Drv, Syscall) ->
    case alcove:define(Drv, [], Syscall) of
        unknown -> [];
        NR -> ?ALLOW_SYSCALL(NR)
    end.

filter(Drv) ->
    Arch = alcove:define(Drv, [], alcove:audit_arch()),
    [
        ?VALIDATE_ARCHITECTURE(Arch),
        ?EXAMINE_SYSCALL,
        allow_syscall(Drv, sys_rt_sigreturn),
        allow_syscall(Drv, sys_sigreturn),
        allow_syscall(Drv, sys_exit_group),
        allow_syscall(Drv, sys_exit),
        allow_syscall(Drv, sys_read),
        allow_syscall(Drv, sys_write),
        allow_syscall(Drv, sys_writev),
        allow_syscall(Drv, sys_setrlimit),
        allow_syscall(Drv, sys_getrlimit),
        allow_syscall(Drv, sys_ugetrlimit),
        allow_syscall(Drv, sys_poll)
    ].

enforce(Drv, Pids, Filter0) ->
    Filter = filter(Drv) ++ [Filter0],

    {ok,_,_,_,_,_} = alcove:prctl(Drv, Pids, pr_set_no_new_privs, 1, 0, 0, 0),

    Pad = (erlang:system_info({wordsize,external}) - 2) * 8,

    Prog = [
        <<(iolist_size(Filter) div 8):2/native-unsigned-integer-unit:8>>,
        <<0:Pad>>,
        {ptr, list_to_binary(Filter)}
    ],
    alcove:prctl(Drv, Pids,
        pr_set_seccomp, seccomp_mode_filter, Prog, 0, 0).
