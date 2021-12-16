@doc clearenv(3): zero process environment

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.176.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,19048}
3> alcove:clearenv(Drv, [Pid]).
ok
4> alcove:environ(Drv, [Pid]).
[]
'''
