@doc chdir(2): change process current working directory

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.176.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,18617}
3> alcove:chdir(Drv, [Pid], "/tmp").
ok
4> alcove:chdir(Drv, [], "/").
ok
5> alcove:getcwd(Drv, [Pid]).
{ok,<<"/tmp">>}
6> alcove:getcwd(Drv, []).
{ok,<<"/">>}
'''
