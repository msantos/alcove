@doc lseek(2): set file offset for read/write

== Examples ==

```
1> {ok, Drv} = alcove_drv:start().
{ok,<0.177.0>}
2> {ok, Pid} = alcove:fork(Drv, []).
{ok,18820}
3> {ok, FD} = alcove:open(Drv, [Pid], "/etc/passwd", [o_rdonly], 0).
{ok,6}
4> alcove:lseek(Drv, [Pid], FD, 0, 0).
ok
'''
