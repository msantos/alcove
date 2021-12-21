@doc sethostname(2): set the system hostname

This function is probably only useful if running in a uts namespace or
a jail.

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.208.0>}
2> {ok, Pid} = alcove:clone(Drv, [], [clone_newuts]).
{ok,44378}
3> alcove:sethostname(Drv, [Pid], "test").
ok
4> alcove:gethostname(Drv, []).
{ok,<<"host1">>}
5> alcove:gethostname(Drv, [Pid]).
{ok,<<"test">>}
'''
