@doc chroot(2): change root directory

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.176.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,19048}
3> alcove:chroot(Drv, [Pid], "/tmp").
ok
4> alcove:chdir(Drv, [Pid], "/").
ok
5> alcove:getcwd(Drv, [Pid]).
{ok,<<"/">>}
'''
