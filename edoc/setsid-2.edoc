@doc setsid(2): create a new session

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28493}
3> alcove:setsid(Drv, [Pid]).
{ok,28493}
'''
