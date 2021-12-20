@doc kill(2): terminate a process

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> alcove:kill(Drv, [], 0, 0).
ok
3> alcove:kill(Drv, [], 12345, 0).
{error,esrch}
4> {ok, Pid} = alcove:fork(Drv, []).
{ok,8288}
5> alcove:kill(Drv, [], Pid, 0).
ok
6> alcove:kill(Drv, [], Pid, sigkill).
ok
7> alcove:kill(Drv, [], Pid, 0).
{error,esrch}
8> flush().
Shell got {alcove_ctl,<0.177.0>,[8288],fdctl_closed}
Shell got {alcove_event,<0.177.0>,[8288],{termsig,sigkill}}
'''
