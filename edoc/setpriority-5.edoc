@doc setpriority(2): set scheduling priority of process, process group or user

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28210}
3> alcove:setpriority(Drv, [Pid], prio_process, 0, 10).
ok
4> alcove:getpriority(Drv, [Pid], prio_process, 0).
{ok,10}
'''
