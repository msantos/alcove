%%% Copyright (c) 2014-2018, Michael Santos <michael.santos@gmail.com>
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
-module(alcove_seccomp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("alcove/include/alcove.hrl").
-include_lib("alcove/include/alcove_seccomp.hrl").

-export([
        all/0,
        init_per_suite/1,
        end_per_suite/1,
        init_per_testcase/2,
        end_per_testcase/2
    ]).
-export([
        kill_prctl/1,
        allow_prctl/1,
        trap_prctl/1,

        kill_seccomp/1,
        allow_seccomp/1,
        trap_seccomp/1
    ]).

all() ->
    case os:type() of
        {unix, linux} ->
            [
                kill_prctl,
                allow_prctl,
                trap_prctl,

                kill_seccomp,
                allow_seccomp,
                trap_seccomp
            ];
        _ ->
            {skip, "seccomp tests are Linux only"}
    end.

init_per_suite(Config) ->
    {ok, Drv} = start_link([]),
    Result = try alcove:prctl_constant(Drv, [], seccomp_mode_filter) of
        unknown ->
            {skip, "seccomp not supported"};

        N when is_integer(N) ->
            Config
    catch
        error:undef ->
            {skip, "seccomp not supported"}
    end,
    alcove_drv:stop(Drv),
    Result.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Test, Config) ->
    Exec = case os:getenv("ALCOVE_TEST_EXEC") of
        false -> "sudo";
        Env -> Env
    end,

    {ok, Drv} = start_link([
            {exec, Exec},
            {maxchild, 8},
            termsig
        ]),

    [{drv, Drv}|Config].

end_per_testcase(_Test, Config) ->
    Drv = ?config(drv, Config),
    alcove_drv:stop(Drv).

%%
%% Tests
%%
kill_prctl(Config) ->
  kill(Config, prctl).

allow_prctl(Config) ->
  allow(Config, prctl).

trap_prctl(Config) ->
  trap(Config, prctl).

kill_seccomp(Config) ->
  kill(Config, seccomp).

allow_seccomp(Config) ->
  allow(Config, seccomp).

trap_seccomp(Config) ->
  trap(Config, seccomp).

% Seccomp filter terminates the process wiith SIGSYS if the system call
% is not allowed.
kill(Config, Syscall) ->
    Drv = ?config(drv, Config),

    {ok, Pid} = alcove:fork(Drv, []),
    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_KILL), Syscall),
    % Allowed: cached by process
    Pid = alcove:getpid(Drv, [Pid]),
    % Not allowed: SIGSYS
    {'EXIT',{{termsig,sigsys},_}} = (catch alcove:getcwd(Drv, [Pid])),

    {error, esrch} = alcove:kill(Drv, [], Pid, 0).

% Seccomp filter matches a whitelist of system calls. Unmatched system
% calls are allowed.
allow(Config, Syscall) ->
    Drv = ?config(drv, Config),

    {ok, Pid} = alcove:fork(Drv, []),
    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_ALLOW), Syscall),
    Pid = alcove:getpid(Drv, [Pid]),
    {ok, _} = alcove:getcwd(Drv, [Pid]),
    ok = alcove:kill(Drv, [], Pid, 0),
    alcove:exit(Drv, [Pid], 0).

% Seccomp filter traps any syscall that is not whitelisted. The system
% call returns an error or a dummy value. The process is not terminated.
trap(Config, Syscall) ->
    Drv = ?config(drv, Config),

    {ok, Pid} = alcove:fork(Drv, []),
    {ok,_} = alcove:sigaction(Drv, [Pid], sigsys, sig_info),

    enforce(Drv, [Pid], ?BPF_STMT(?BPF_RET+?BPF_K, ?SECCOMP_RET_TRAP), Syscall),

    % Allowed: cached by process
    Pid = alcove:getpid(Drv, [Pid]),
    % Not allowed: SIGSYS
    true = case alcove:getcwd(Drv, [Pid]) of
        {error,unknown} -> true;
        {ok,<<>>} -> true;
        Cwd -> {false, Cwd}
    end,

    {signal, sigsys, _} = receive
        {alcove_event,Drv,[Pid],Event} ->
            Event
    after
        2000 ->
            timeout
    end,

    ok = alcove:kill(Drv, [], Pid, 0),
    alcove:exit(Drv, [Pid], 0).


allow_syscall(Drv, Syscall) ->
    try alcove:define(Drv, [], Syscall) of
        NR -> ?ALLOW_SYSCALL(NR)
    catch
        error:{unknown, Syscall} -> []
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
        allow_syscall(Drv, sys_getpid),
        allow_syscall(Drv, sys_read),
        allow_syscall(Drv, sys_write),
        allow_syscall(Drv, sys_writev),
        allow_syscall(Drv, sys_setrlimit),
        allow_syscall(Drv, sys_getrlimit),
        allow_syscall(Drv, sys_ugetrlimit),
        allow_syscall(Drv, sys_poll)
    ].

enforce(Drv, Pids, Filter0, Syscall) ->
    Filter = filter(Drv) ++ [Filter0],

    {ok,_,_,_,_,_} = alcove:prctl(Drv, Pids, pr_set_no_new_privs, 1, 0, 0, 0),

    Pad = (erlang:system_info({wordsize,external}) - 2) * 8,

    Prog = [
        <<(iolist_size(Filter) div 8):2/native-unsigned-integer-unit:8>>,
        <<0:Pad>>,
        {ptr, list_to_binary(Filter)}
    ],

    case Syscall of
      prctl ->
        {ok,_,_,_,_,_} = alcove:prctl(Drv, Pids, pr_set_seccomp,
                                      seccomp_mode_filter, Prog, 0, 0);

      seccomp ->
        ok = alcove:seccomp(Drv, Pids, seccomp_set_mode_filter, 0, Prog)
    end,

    ok.

start_link(Opt) ->
    Ctldir = case os:getenv("ALCOVE_TEST_CTLDIR") of
               false -> [];
               Dir -> [{ctldir, Dir}]
             end,

    alcove_drv:start_link(Opt ++ Ctldir).
