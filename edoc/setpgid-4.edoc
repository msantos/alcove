@doc setpgid(2): set process group

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28210}
3> alcove:setpgid(Drv, [Pid], 0, 0).
ok
'''
