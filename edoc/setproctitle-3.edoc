@doc setproctitle(3): set the process title

Overwrites arg0.

On Linux, prctl/6,7 can be used to set the command name:

```
{ok,Fork} = alcove:fork(Drv, []),
alcove:prctl(Drv, [Fork], pr_set_name, <<"pseudonym">>, 0,0,0).
'''

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28210}
3> alcove:setproctitle(Drv, [Pid], "new process name").
ok
'''
