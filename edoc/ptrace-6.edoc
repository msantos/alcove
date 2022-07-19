@doc ptrace(2): process trace

== Examples ==

```
-module(ptrace).

-export([run/0]).

run() ->
    {ok, Drv} = alcove_drv:start_link(),
    {ok, Pid1} = alcove:fork(Drv, []),

    {ok, Pid2} = alcove:fork(Drv, [Pid1]),

    % disable the alcove event loop: child process must be managed by
    % the caller
    {ok, sig_dfl} = alcove:sigaction(Drv, [Pid1], sigchld, sig_info),

    % enable ptracing in the child process and exec() a command
    {ok, 0, <<>>, <<>>} = alcove:ptrace(
        Drv,
        [Pid1, Pid2],
        ptrace_traceme,
        0,
        0,
        0
    ),
    ok = alcove:execvp(Drv, [Pid1, Pid2], "cat", ["cat"]),

    % the parent is notified
    {signal, sigchld, _} = alcove:event(Drv, [Pid1], 5000),
    {ok, Pid2, _, [{stopsig, sigtrap}]} = alcove:waitpid(
        Drv,
        [Pid1],
        -1,
        [wnohang]
    ),

    % should be no other events
    {ok, 0, 0, []} = alcove:waitpid(Drv, [Pid1], -1, [wnohang]),

    % allow the process to continue
    {ok, 0, <<>>, <<>>} = alcove:ptrace(Drv, [Pid1], ptrace_cont, Pid2, 0, 0),

    ok = alcove:stdin(Drv, [Pid1, Pid2], "test\n"),

    ok =
        receive
            {alcove_stdout, Drv, [Pid1, Pid2], <<"test\n">>} ->
                ok
        after 5000 -> timeout
        end,

    % Send a SIGTERM and re-write it to a harmless SIGWINCH
    ok = alcove:kill(Drv, [Pid1], Pid2, sigterm),
    {signal, sigchld, _} = alcove:event(Drv, [Pid1], 5000),
    {ok, Pid2, _, [{stopsig, sigterm}]} = alcove:waitpid(
        Drv,
        [Pid1],
        -1,
        [wnohang]
    ),

    {ok, 0, <<>>, <<>>} = alcove:ptrace(
        Drv,
        [Pid1],
        ptrace_cont,
        Pid2,
        0,
        28
    ),

    % Convert a SIGWINCH to SIGTERM
    ok = alcove:kill(Drv, [Pid1], Pid2, sigwinch),
    {signal, sigchld, _} = alcove:event(Drv, [Pid1], 5000),
    {ok, 0, <<>>, <<>>} = alcove:ptrace(
        Drv,
        [Pid1],
        ptrace_cont,
        Pid2,
        0,
        15
    ),
    {ok, Pid2, _, [{termsig, sigterm}]} = alcove:waitpid(
        Drv,
        [Pid1],
        -1,
        []
    ).
'''
