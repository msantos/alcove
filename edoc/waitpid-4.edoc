@doc waitpid(2): wait for process to change state

Process state changes are handled by the alcove SIGCHLD event handler
by default. To use waitpid/4,5, disable the signal handler:

```
alcove:sigaction(Drv, [Pid], sigchld, sig_info)
'''

Note: if the default SIGCHLD handler is disabled, waitpid/4,5 should be
called to reap zombie processes:

```
alcove:waitpid(Drv, [], -1, [wnohang])
'''

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28978}
3> alcove:sigaction(Drv, [Pid], sigchld, sig_info).
{ok,sig_dfl}
4> alcove:execvp(Drv, [Pid], "sleep", ["sleep", "20"]).
ok
5> alcove:waitpid(Drv, [], -1, []).
{ok,28978,0,[{exit_status,0}]}
'''
