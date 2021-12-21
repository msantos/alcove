@doc setresuid(2): set real, effective and saved user ID

== Support ==

* Linux

* FreeBSD

* OpenBSD

== Examples ==

```
1> {ok, Drv} = alcove_drv:start([{exec, "sudo -n"}]).
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,28493}
3> alcove:setresuid(Drv, [Pid], 123, 123, 123).
ok
'''
