@doc cap_fcntls_get(2): get allowed fcntl commands in capability mode

== Support ==

* FreeBSD

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.209.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,77853}
3> {ok, FD} = alcove:open(Drv, [Pid], "/etc/passwd", [o_rdonly], 0).
{ok,6}
4> ok = alcove:cap_enter(Drv, [Pid]).
ok
5> alcove:cap_fcntls_get(Drv, [Pid], FD).
{ok,120}
'''
