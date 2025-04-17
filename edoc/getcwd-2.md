@doc getcwd(3): return the current working directory

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,3925}
3> alcove:chdir(Drv, [Pid], "/").
ok
4> alcove:getcwd(Drv, [Pid]).
{ok,<<"/">>}
'''
