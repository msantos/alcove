@doc fork(2): create a new process

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.189.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,18721}
3> alcove:getpid(Drv, [Pid]).
18721
'''
